#|
 This file is a part of audio-blender
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.audio-blender)

(defvar *dispatch-functions* (make-hash-table :test 'eql))

(defun dispatch-function (name &optional (error T))
  (or (gethash name *dispatch-functions*)
      (when error (error "No such dispatch function ~s." name))))

(defun (setf dispatch-function) (function name)
  (setf (gethash name *dispatch-functions*) function))

(defun remove-dispatch-function (name)
  (remhash name *dispatch-functions*)
  (fmakunbound name))

(defclass dispatch-function ()
  ((name :initarg :name :initform NIL :accessor name)
   (args :initarg :args :initform NIL :accessor args)
   (cases :initform NIL :accessor cases)))

(defmethod shared-initialize :after ((function dispatch-function) slots &key cases)
  (when cases
    (setf (cases function) (sort cases #'subtypep :key #'first))))

(defmethod compile-dispatch-function ((function dispatch-function))
  (with-slots (name args cases) function
    (compile name `(lambda ,args
                     (declare (ignorable ,@args))
                     (typecase ,(first args)
                       ,@(loop for (type cargs . body) in cases
                               collect `(,type
                                         (let ,(mapcar #'list args cargs)
                                           ,@body)))
                       (T (error "Cannot dispatch to ~s, not one of ~a." ,(first args) ',(mapcar #'first cases))))))
    function))

(defmethod compile-dispatch-function ((name symbol))
  (compile-dispatch-function (dispatch-function name)))

(defmethod compile-dispatch-function ((name cons))
  (compile-dispatch-function (dispatch-function name)))

(defun update-or-create-dispatch-function (name args &optional cases)
  (let ((prev (dispatch-function name NIL)))
    (compile-dispatch-function
     (cond (prev
            (reinitialize-instance prev :args args :cases cases))
           (T
            (setf (dispatch-function name)
                  (make-instance 'dispatch-function :name name :args args :cases cases)))))))

(defmacro define-dispatcher (name args &body cases)
  `(progn (declaim (inline ,name))
          (update-or-create-dispatch-function ',name ',args ',cases)
          ',name))

(defmacro define-dispatch-method (name type args &body body)
  (let ((fun (gensym "FUN")))
    `(let ((,fun (dispatch-function ',name)))
       (pushnew '(,type ,args ,@body) (cases ,fun) :key #'first)
       (compile-dispatch-function ,fun))))
