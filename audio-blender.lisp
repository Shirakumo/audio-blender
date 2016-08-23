#|
 This file is a part of audio-blender
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.audio-blender)

(defun ch-var (channel)
  (first channel))

(defun ch-type (channel)
  (second channel))

(defun c-converter (from to val)
  (when (not (or (eql from :float) (eql to :float)))
    (error "Don't know how to convert from ~a to ~a." from to))
  (ecase from
    (:float (case to
              (:float  `(float->fl ,val))
              (:double `(coerce (the double-float ,val) 'single-float))
              (:uint8  `(float->ub ,val 8))
              (:uint16 `(float->ub ,val 16))
              (:uint32 `(float->ub ,val 32))
              (:int8   `(float->sb ,val 8))
              (:int16  `(float->sb ,val 16))
              (:int32  `(float->sb ,val 32))))
    (:double `(coerce (the double-float ,val) 'single-float))
    (:uint8  `(ub->float ,val 8))
    (:uint16 `(ub->float ,val 16))
    (:uint32 `(ub->float ,val 32))
    (:int8   `(sb->float ,val 8))
    (:int16  `(sb->float ,val 16))
    (:int32  `(sb->float ,val 32))))

(defmacro define-mixer (name (out &rest channels) &body body)
  (let ((size (gensym "SIZE"))
        (i (gensym "I"))
        (ch-vars (mapcar #'ch-var channels)))
    `(defun ,name (,size ,(ch-var out) ,@ch-vars)
       (declare (optimize speed))
       (declare (type cffi:foreign-pointer ,(ch-var out) ,@ch-vars))
       (declare (type fixnum ,size))
       (dotimes (,i ,size ,(ch-var out))
         (declare (type fixnum ,i))
         (let ,(loop for (var type) in channels
                     collect `(,var ,(c-converter type :float `(cffi:mem-aref ,var ,type ,i))))
           (declare (type single-float ,@ch-vars))
           (setf (cffi:mem-aref ,(ch-var out) ,(ch-type out) ,i)
                 ,(c-converter :float (ch-type out)
                               `(progn ,@body))))))))

(defun mix/ (&rest channels)
  (let ((sum 0.0s0) (len 0.0s0))
    (dolist (c channels (sf! (/ sum len)))
      (incf len 1.0s0) (incf sum (sf! c)))))

(define-compiler-macro mix/ (&rest channels)
  `(sf!
        ,(case (length channels)
           (0 0.0s0)
           (1 (first channels))
           (T `(/ (+ ,@channels)
                  ,(sf (length channels)))))))

(defun mix+ (&rest channels)
  (let ((sum 0.0s0))
    (dolist (c channels (sf! sum))
      (incf sum (sf! c)))))

(define-compiler-macro mix+ (&rest channels)
  `(sf! ,(case (length channels)
           (0 0.0s0)
           (1 (first channels))
           (T `(+ ,@channels)))))

(defun mix_ (&rest channels)
  (let ((sum 0.0s0))
    (dolist (c channels (sf! (clamp -1.0s0 sum 1.0s0)))
      (incf sum (sf! c)))))

(define-compiler-macro mix_ (&rest channels)
  `(sf! (clamp -1.0s0 (mix+ ,@channels) +1.0s0)))

(defun amp (factor channel)
  (sf (* factor channel)))

(define-compiler-macro amp (&whole whole &environment env factor channel)
  (if (constantp factor env)
      `(sf! (* ,(sf factor) ,channel))
      whole))

(defun cap (factor channel)
  (sf (clamp (- factor) channel (+ factor))))

(define-compiler-macro cap (&whole whole &environment env factor channel)
  (if (constantp factor env)
      (let ((factor (sf factor)))
        `(sf! (clamp ,(- factor) ,channel ,(+ factor))))
      whole))
