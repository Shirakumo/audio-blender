#|
 This file is a part of audio-blender
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.audio-blender)

(defvar *mixers* (make-hash-table :test 'eql))

(defun enlist (a &rest vals)
  (if (listp a) a (list* a vals)))

(defun delist (a &optional (key #'first))
  (if (listp a) (funcall key a) a))

(defun mixer (name &optional (error T))
  (or (gethash name *mixers*)
      (when error (error "No such mixer ~s." name))))

(defun (setf mixer) (constructor name)
  (setf (gethash name *mixers*) constructor))

(defun remove-mixer (name)
  (remhash name *mixers*))

(defun make-mixer (name out &rest channels)
  (apply (mixer name) out channels))

(defmacro with-mixer ((name out &rest channels) &body body)
  `(let ((,name (make-mixer ',name ,out ,@channels)))
     ,@body))

(defun make-mixer-lambda (out channels extra-vars body)
  (let ((size (gensym "SIZE"))
        (i (gensym "I")))
    `(lambda (,size ,@extra-vars)
       (declare (type fixnum ,size))
       (dotimes (,i ,size ,out)
         (let ,(loop for (var chan) in channels
                     collect `(,var ,(sample chan i)))
           ,(setf (sample out i)
                  `(progn ,@body)))))))

(defmacro define-mixer (name channels extra-vars &body body)
  (let ((out (gensym "OUT")))
    `(progn (setf (mixer ',name)
                  (lambda (,out ,@channels)
                    (compile NIL (make-mixer-lambda
                                  ,out
                                  (list ,@(loop for chan in channels
                                                collect `(list ',chan ,chan)))
                                  ',extra-vars
                                  ',body))))
            ',name)))
