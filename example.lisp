#|
 This file is a part of audio-blender
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:audio-blender-example
  (:nicknames #:org.shirakumo.fraf.audio-blender.example)
  (:use #:cl))
(in-package #:org.shirakumo.fraf.audio-blender.example)

(audio-blender::define-mixer test (out a b)
  a)

(defclass mpg-channel (audio-blender::channel)
  ((file :accessor file))
  (:default-initargs
   :sample-type :float))

(defmethod initialize-instance :after ((channel mpg-channel) &key path)
  (setf (file channel) (cl-mpg123:connect
                        (cl-mpg123:make-file path :accepted-format `(44100 :stereo ,(audio-blender::sample-type channel))))))

(defmethod audio-blender::buffer ((channel mpg-channel))
  (cl-mpg123:buffer (file channel)))

(defmethod audio-blender::buffer-size ((channel mpg-channel))
  (cl-mpg123:buffer-size (file channel)))

(defmethod audio-blender::refresh ((channel mpg-channel) max)
  (handler-bind ((cl-mpg123:read-failed (lambda (err)
                                          (when (eql (cl-mpg123:error-code err) :done)
                                            (return-from audio-blender::refresh 0)))))
    (cl-mpg123:read-directly (file channel) (audio-blender::buffer channel) max)))

(defmethod audio-blender::sample ((channel mpg-channel) pos)
  `(cffi:mem-aref ,(audio-blender::buffer channel) ,(audio-blender::sample-type channel) ,pos))

(defmethod disconnect ((mpg mpg-channel))
  (cl-mpg123:disconnect (file mpg)))

(defclass out-channel (audio-blender::c-array-channel)
  ((output :accessor output))
  (:default-initargs
   :sample-type :float))

(defmethod initialize-instance :after ((channel out-channel) &key driver)
  (setf (output channel) (cl-out123:connect
                          (cl-out123:make-output driver :rate 44100 :channels 2 :encoding (audio-blender::sample-type channel)))))

(defmethod audio-blender::refresh ((channel out-channel) max)
  (cl-out123:play (output channel) (audio-blender::buffer channel) max))

(defmethod (setf audio-blender::sample) (sample (channel out-channel) pos)
  `(setf (cffi:mem-aref ,(audio-blender::buffer channel) ,(audio-blender::sample-type channel) ,pos)
         ,sample))

(defmethod start ((out out-channel))
  (cl-out123:start (output out)))

(defmethod disconnect ((out out-channel))
  (cl-out123:disconnect (output out)))

(defun main (file-a file-b &key output-driver)
  (let* ((chn-a (make-instance 'mpg-channel :path file-a))
         (chn-b (make-instance 'mpg-channel :path file-b))
         (chn-o (make-instance 'out-channel :driver output-driver
                                            :buffer-size (audio-blender::buffer-size chn-a)))
         (mixer (audio-blender::make-mixer 'test chn-o chn-a chn-b)))
    ;(start chn-o)
    (unwind-protect
         (loop for a-size = (audio-blender::refresh chn-a (audio-blender::buffer-size chn-a))
               for b-size = (audio-blender::refresh chn-b (audio-blender::buffer-size chn-a))
               until (and (= 0 a-size) (= 0 b-size))
               do (funcall mixer (max a-size b-size))
                  (break)
                  ;(audio-blender::refresh chn-o (max a-size b-size))
                  (format T "~&Read ~a ~a" a-size b-size))
      (disconnect chn-o)
      (disconnect chn-a)
      (disconnect chn-b))))
