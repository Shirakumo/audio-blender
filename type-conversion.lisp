#|
 This file is a part of audio-blender
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.audio-blender)

(defmacro sf! (thing)
  `(the single-float ,thing))

(declaim (inline sf))
(defun sf (thing)
  (sf! (coerce thing 'single-float)))

(declaim (inline clamp))
(defun clamp (min a max)
  (if (<= a min)
      min
      (if (<= max a)
          max
          a)))

(defmacro with-size-check ((size whole &optional env) &body body)
  `(cond ((not (constantp ,size ,env))
          ,whole)
         ((typep ,size '(integer 1 32))
          ,@body)
         (T (error "Can't convert ~s bits of size." ,size))))

(defun sb->float (a size)
  (declare (optimize speed))
  (declare (type (integer 1 32) size))
  (declare (type (signed-byte 32) a))
  (/ (sf a)
     (truncate (ash 1 size) 2)))

(define-compiler-macro sb->float (&whole whole &environment env a size)
  (with-size-check (size whole env)
    `(/ (sf ,a)
        ,(sf (truncate (ash 1 size) 2)))))

(defun ub->float (a size)
  (declare (optimize speed))
  (declare (type (integer 1 32) size))
  (declare (type (unsigned-byte 32) a))
  (let ((half (sf (truncate (ash 1 size) 2))))
    (/ (- (sf a) half)
       half)))

(define-compiler-macro ub->float (&whole whole &environment env a size)
  (with-size-check (size whole env)
    (let ((half (sf (truncate (ash 1 size) 2))))
      `(/ (- (sf ,a) ,half)
          ,half))))

(defun float->sb (a size)
  (declare (optimize speed))
  (declare (type (integer 1 32) size))
  (declare (type single-float a))
  (let ((half (truncate (ash 1 size) 2)))
    (clamp (- half) (the (signed-byte 64) (truncate (* a half))) (1- half))))

(define-compiler-macro float->sb (&whole whole &environment env a size)
  (with-size-check (size whole env)
    (let ((half (truncate (ash 1 size) 2)))
      `(clamp ,(- half) (the (signed-byte 64) (truncate (* ,a ,half))) ,(1- half)))))

(defun float->ub (a size)
  (declare (optimize speed))
  (declare (type (integer 1 32) size))
  (declare (type single-float a))
  (let* ((full (ash 1 size))
         (half (truncate full 2)))
    (clamp 0 (the (signed-byte 64) (truncate (* (+ 1.0s0 a) half))) (1- full))))

(define-compiler-macro float->ub (&whole whole &environment env a size)
  (with-size-check (size whole env)
    (let* ((full (ash 1 size))
           (half (truncate full 2)))
      `(clamp 0 (the (signed-byte 64) (truncate (* (+ 1.0s0 ,a) ,half))) ,(1- full)))))

(defun float->fl (a)
  (declare (optimize speed))
  (declare (type single-float a))
  (sf! (clamp -1.0s0 a 1.0s0)))

(defun ensure-ctypename (ltype)
  (cond ((keywordp ltype)
         ltype)
        ((not (listp ltype))
         (ecase ltype
           (single-float :float)
           (double-float :double)))
        ((eql (first ltype) 'unsigned-byte)
         (ecase (second ltype)
           (8 :uint8)
           (16 :uint16)
           (32 :uint32)))
        ((eql (first ltype) 'signed-byte)
         (ecase (second ltype)
           (8 :int8)
           (16 :int16)
           (32 :int32)))
        (T
         (error "Cannot convert ~s to ctypename." ltype))))

(defun coerce-ctype (val from to)
  (ecase from
    (:float
     (case to
       (:float  (float->fl val))
       (:double (coerce (the double-float val) 'single-float))
       (:uint8  (float->ub val 8))
       (:uint16 (float->ub val 16))
       (:uint32 (float->ub val 32))
       (:int8   (float->sb val 8))
       (:int16  (float->sb val 16))
       (:int32  (float->sb val 32))))
    (:double
     (case to
       (:float (coerce (the double-float val) 'single-float))
       (T (coerce-ctype (coerce-ctype val :double :float) :float to))))
    (:uint8
     (case to
       (:float (ub->float val 8))
       (T (coerce-ctype (coerce-ctype val :uint8 :float) :float to))))
    (:uint16
     (case to
       (:float (ub->float val 16))
       (T (coerce-ctype (coerce-ctype val :uint16 :float) :float to))))
    (:uint32
     (case to
       (:float (ub->float val 32))
       (T (coerce-ctype (coerce-ctype val :uint32 :float) :float to))))
    (:int8
     (case to
       (:float (sb->float val 8))
       (T (coerce-ctype (coerce-ctype val :int8 :float) :float to))))
    (:int16
     (case to
       (:float (sb->float val 16))
       (T (coerce-ctype (coerce-ctype val :int16 :float) :float to))))
    (:int32
     (case to
       (:float (sb->float val 32))
       (T (coerce-ctype (coerce-ctype val :int32 :float) :float to))))))

(define-compiler-macro coerce-ctype (&whole whole &environment env val from to)
  (cond ((and (constantp from env) (constantp to env))
         (ecase from
           (:float
            (case to
              (:float  `(float->fl ,val))
              (:double `(coerce (the double-float ,val) 'single-float))
              (:uint8  `(float->ub ,val 8))
              (:uint16 `(float->ub ,val 16))
              (:uint32 `(float->ub ,val 32))
              (:int8   `(float->sb ,val 8))
              (:int16  `(float->sb ,val 16))
              (:int32  `(float->sb ,val 32))))
           (:double
            (case to
              (:float `(coerce (the double-float ,val) 'single-float))
              (T `(coerce-ctype (coerce-ctype ,val :double :float) :float to))))
           (:uint8
            (case to
              (:float `(ub->float ,val 8))
              (T `(coerce-ctype (coerce-ctype ,val :uint8 :float) :float to))))
           (:uint16
            (case to
              (:float `(ub->float ,val 16))
              (T `(coerce-ctype (coerce-ctype ,val :uint16 :float) :float to))))
           (:uint32
            (case to
              (:float `(ub->float ,val 32))
              (T `(coerce-ctype (coerce-ctype ,val :uint32 :float) :float to))))
           (:int8
            (case to
              (:float `(sb->float ,val 8))
              (T `(coerce-ctype (coerce-ctype ,val :int8 :float) :float to))))
           (:int16
            (case to
              (:float `(sb->float ,val 16))
              (T `(coerce-ctype (coerce-ctype ,val :int16 :float) :float to))))
           (:int32
            (case to
              (:float `(sb->float ,val 32))
              (T `(coerce-ctype (coerce-ctype ,val :int32 :float) :float to))))))
        (T
         whole)))

(deftype c-array (type)
  (declare (ignore type))
  'cffi:foreign-pointer)

(defun ctype-zero (ctype)
  (case ctype
    (:float 0.0s0) (:double 0.0d0)
    ((:int8 :int16 :int32) 0)
    (:uint8 #x80)
    (:uint16 #x8000)
    (:uint32 #x80000000)))
