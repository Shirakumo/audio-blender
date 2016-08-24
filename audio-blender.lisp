#|
 This file is a part of audio-blender
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.audio-blender)

(defun c-converter (from to val)
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

(defun make-mixer (out out-type channels channel-types body)
  (let ((size (gensym "SIZE-"))
        (i (gensym "I-")))
    `(lambda (,size ,out ,@channels)
       (declare (optimize speed))
       (declare (type cffi:foreign-pointer ,out ,@channels))
       (declare (type fixnum ,size))
       (dotimes (,i ,size ,out)
         (declare (type fixnum ,i))
         (let ,(loop for channel in channels
                     for type in channel-types
                     collect `(,channel ,(c-converter type :float `(cffi:mem-aref ,channel ,type ,i))))
           (declare (type single-float ,@channels))
           (setf (cffi:mem-aref ,out ,out-type ,i)
                 ,(c-converter :float out-type
                               `(progn ,@body))))))))

(defmacro define-mixer (name (out &rest channels) &body body)
  `(defun ,name (,out ,@channels)
     (compile NIL (make-mixer ',out ,out ',channels (list ,@channels)
                              ',body))))

(defstruct (aggregate-channel
            (:constructor %make-aggregate-channel))
  (channels    (make-array 0 :initial-element NIL) :type vector)
  (buffer      NIL :type cffi:foreign-pointer :read-only T)
  (buffer-size 0   :type fixnum :read-only T)
  (lock        (bt:make-lock "aggregate-channel lock") :read-only T))

(defun add-aggregate-channel (channel aggregate)
  (bt:with-lock-held ((aggregate-channel-lock aggregate))
    (let ((prev (aggregate-channel-channels aggregate)))
      (unless (find channel prev)
        (let ((channels (make-array (1+ (length prev)) :element-type 'cffi:foreign-pointer :initial-element channel)))
          (dotimes (i (length prev))
            (setf (aref channels (1+ i)) (aref prev i)))
          (setf (aggregate-channel-channels aggregate) channels)))))
  aggregate)

(defun remove-aggregate-channel (channel aggregate)
  (bt:with-lock-held ((aggregate-channel-lock aggregate))
    (let ((prev (aggregate-channel-channels aggregate)))
      (when (find channel prev)
        (let ((channels (make-array (1- (length prev)) :element-type 'cffi:foreign-pointer :initial-element channel)))
          (loop for i from 0 below (length channels)
                for chan across prev
                do (unless (eql channel chan)
                     (setf (aref channels i) chan)))
          (setf (aggregate-channel-channels aggregate) channels)))))
  aggregate)

(defun make-aggregate-channel (buffer-size &rest channels)
  (let* ((channels  (make-array (length channels) :element-type 'cffi:foreign-pointer :initial-contents channels))
         (buffer    (cffi:foreign-alloc :unsigned-char :count buffer-size))
         (aggregate (%make-aggregate-channel :channels channels :buffer buffer :buffer-size buffer-size)))
    (tg:finalize aggregate (lambda () (cffi:foreign-free buffer)))
    aggregate))
