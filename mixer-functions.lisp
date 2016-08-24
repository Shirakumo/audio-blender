#|
 This file is a part of audio-blender
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.audio-blender)

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

(defun call (function channel)
  (sf (funcall function channel)))

(define-compiler-macro call (&whole whole &environment env function channel)
  (if (and (constantp function env)
           (listp function)
           (or (eql (first function) 'quote)
               (eql (first function) 'function))
           (symbolp (second function)))
      `(sf (,(second function) ,channel))
      whole))
