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

(defun ensure-ctype (ltype)
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
         (error "Cannot convert ~s to ctype." ltype))))

(defun c-converter (val from to)
  (when (not (or (eql from :float) (eql to :float)))
    (error "Don't know how to convert from ~a to ~a." from to))
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
    (:double `(coerce (the double-float ,val) 'single-float))
    (:uint8  `(ub->float ,val 8))
    (:uint16 `(ub->float ,val 16))
    (:uint32 `(ub->float ,val 32))
    (:int8   `(sb->float ,val 8))
    (:int16  `(sb->float ,val 16))
    (:int32  `(sb->float ,val 32))))

(deftype c-array (type)
  (declare (ignore type))
  'cffi:foreign-pointer)
