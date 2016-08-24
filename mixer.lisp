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
           (destructuring-bind (var type &optional (offset 0)) spec
             `(list ,var ',type ,offset))))
    `(let ((,name (make-mixer ',name ,(c out) ,@(mapcar #'c channels))))
       ,@body)))

(defmacro channel-aref (channel type i)
  (let ((eltype (ensure-ctype (second type))))
    (c-converter eltype :float (case (first type)
                                 (c-array `(cffi:mem-aref ,channel ,eltype ,i))
                                 (vector `(aref ,channel ,i))))))

(defmacro set-channel-aref (channel type i var)
  (let ((eltype (ensure-ctype (second type))))
    `(setf ,(case (first type)
              (c-array `(cffi:mem-aref ,channel ,eltype ,i))
              (vector `(aref ,channel ,i)))
           ,(c-converter eltype :float var))))

(defun make-mixer-function (out channels body)
  (let ((size (gensym "SIZE"))
        (i (gensym "I"))
        (sample (gensym "SAMPLE")))
    `(lambda (,size)
       (declare (type fixnum ,size))
       (let ,(mapcar #'butlast (cons out channels))
         ,@(loop for (var NIL type) in (cons out channels)
                 collect `(declare (type ,type ,var)))
         (dotimes (,i ,size ,(first out))
           (declare (type fixnum ,i))
           (let ,(loop for (var NIL type) in channels
                       collect `(,var (channel-aref ,var ,type ,i)))
             (let ((,sample (progn ,@body)))
               (set-channel-aref ,(first out) ,(third out) ,i ,sample))))))))

(defun make-mixer-definition (out channels body)
  (lambda (outd &rest channeld)
    (let ((out (cons out outd))
          (channels (mapcar #'cons channels channeld)))
      (compile NIL (make-mixer-function out channels body)))))

(defmacro define-mixer (name (out &rest channels) &body body)
  `(progn (setf (mixer ',name)
                (make-mixer-definition ',out ',channels ',body))
          ',name))
