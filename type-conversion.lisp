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
