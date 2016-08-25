#|
 This file is a part of audio-blender
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.audio-blender)

(defvar *mixers* (make-hash-table :test 'eql))

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
  (flet ((c (spec)
           (destructuring-bind (var &optional type) (if (listp spec) spec (list spec))
             `(list ,var ',type))))
    `(let ((,name (make-mixer ',name ,(c out) ,@(mapcar #'c channels))))
       ,@body)))

(defmacro define-mixer (name (out &rest channels) &body body)
  (let ((size (gensym "SIZE"))
        (i (gensym "I")))
    `(progn (setf (mixer ',name)
                  (lambda (,out ,@channels)
                    (let ,(loop for chan in (cons out channels)
                                ;; For now we discard the extra information.
                                collect `(,chan (first ,out)))
                      (lambda (,size)
                        (declare (type fixnum ,size))
                        (dotimes (,i ,size ,out)
                          (let ,(loop for chan in channels
                                      collect `(,chan (sample ,chan ,i)))
                            (setf (sample ,out ,i)
                                  (progn ,@body))))))))
            ',name)))
