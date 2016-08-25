#|
 This file is a part of audio-blender
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.audio-blender)

(defclass channel ()
  ((sample-type :reader sample-type)))

(defmethod initialize-instance :after ((channel channel) &key sample-type)
  (setf (slot-value channel 'sample-type)
        (ensure-ctypename sample-type)))

(defgeneric refresh (channel max))
(defgeneric sample (channel pos))
(defgeneric float-sample (channel pos))
(defgeneric (setf sample) (sample channel pos))
(defgeneric (setf float-sample) (float channel pos))

(defmethod float-sample ((channel channel) pos)
  `(coerce-ctype ,(sample channel pos) ,(sample-type channel) :float))

(defmethod (setf float-sample) (float (channel channel) pos)
  `(setf (sample ,channel ,pos) (coerce-ctype ,float :float ,(sample-type channel))))

(defclass buffer-channel (channel)
  ((buffer :initarg :buffer :reader buffer)
   (buffer-fill :initarg :buffer-fill :accessor buffer-fill))
  (:default-initargs
   :buffer (error "BUFFER required.")
   :buffer-fill 0))

(defmethod refresh :around ((channel buffer-channel) max)
  (declare (optimize speed))
  (let ((prev (the fixnum (buffer-fill channel)))
        (new (the fixnum (call-next-method))))
    ;; Backfill with zeroes.
    (when (< new prev)
      (loop with zero = (ctype-zero (sample-type channel))
            for i from new below prev
            do (setf (sample channel i) zero)))
    (setf (buffer-fill channel) new)))

(defgeneric buffer-size (channel))

(defclass c-array-channel (buffer-channel)
  ((buffer-size :initarg :buffer-size :reader buffer-size))
  (:default-initargs
   :buffer NIL
   :buffer-size (error "BUFFER-SIZE required.")))

(defmethod initialize-instance :after ((channel c-array-channel) &key buffer buffer-size)
  (unless buffer
    (let ((buffer (cffi:foreign-alloc :unsigned-char :count buffer-size :initial-element 0)))
      (setf (slot-value channel 'buffer) buffer)
      (tg:finalize channel (lambda () (cffi:foreign-free buffer))))))

(defmethod sample ((channel c-array-channel) pos)
  (declare (type fixnum pos) (optimize speed))
  `(cffi:mem-aref (the cffi:foreign-pointer ,(buffer channel)) ,(sample-type channel) ,pos))

(defmethod (setf sample) (sample (channel c-array-channel) pos)
  (declare (type fixnum pos))
  `(setf (cffi:mem-aref (the cffi:foreign-pointer ,(buffer channel)) ,(sample-type channel) ,pos) ,sample))

(defclass vector-channel (buffer-channel)
  ())

(defmethod buffer-size ((channel vector-channel))
  (length (the vector (buffer channel))))

(defmethod sample ((channel vector-channel) pos)
  (declare (type fixnum pos))
  `(aref (the vector ,(buffer channel)) ,pos))

(defmethod (setf sample) (sample (channel vector-channel) pos)
  (declare (type fixnum pos))
  `(setf (aref (the vector ,(buffer channel)) ,pos) ,sample))

(defclass reading-channel (buffer-channel)
  ((reader :initarg :reader :reader reader))
  (:default-initargs
   :reader (error "READER required.")))

(defmethod refresh ((channel reading-channel) max)
  (declare (type fixnum max))
  (funcall (the function (reader channel))
           (buffer channel)
           (min max (the fixnum (buffer-size channel)))))

(defclass c-array-reading-channel (c-array-channel reading-channel)
  ())

(defclass aggregate-channel (vector-channel)
  ((channels :accessor channels))
  (:default-initargs
   :buffer (make-array 0 :element-type 'single-float :initial-element 0.0s0 :adjustable T)))

(defmethod initialize-instance :after ((channel aggregate-channel) &key channels)
  (setf (channels channel) (coerce channels 'vector)))

(defmethod mix ((aggregate aggregate-channel) max)
  (declare (optimize speed))
  (declare (type fixnum max))
  (let* ((buffer (buffer aggregate))
         (channels (channels aggregate)))
    (declare (type (vector single-float) buffer))
    (declare (type simple-vector channels))
    (dolist (i max buffer)
      (declare (type fixnum i))
      (setf (aref buffer i)
            (/ (let ((sum 0.0s0))
                 (declare (type single-float sum))
                 (dotimes (j (length channels) sum)
                   (incf sum (sf! (sample (aref channels j) i)))))
               (coerce (length channels) 'single-float))))))

(defmethod refresh ((aggregate aggregate-channel) max)
  (declare (optimize speed))
  (let ((channels (channels aggregate)))
    (declare (type simple-vector channels))
    (case (length channels)
      (0 0)
      (T (loop for channel across channels
               for size = (the fixnum (refresh channel max))
               do (when (= 0 size)
                    (remove-channel channel aggregate)))
       (mix aggregate max)
       max))))

(defmethod remove-channel ((channel channel) (aggregate aggregate-channel))
  (let ((channels (remove channel (channels aggregate))))
    ;; Adjust buffer if necessary
    (when (= (buffer-size aggregate)
             (buffer-size channel))
      (adjust-array (buffer aggregate)
                    (loop for c across channels minimize (buffer-size c))
                    :initial-element 0.0s0))
    (setf (channels aggregate) channels))
  aggregate)

(defmethod add-channel ((channel channel) (aggregate aggregate-channel))
  (let ((original (channels aggregate)))
    (unless (find channel original)
      (let ((channels (make-array (1+ (length original)))))
        ;; Refill the array
        (dotimes (i (length original))
          (setf (aref channels (1+ i)) (aref original i)))
        (setf (aref channels 0) channel)
        ;; Adjust buffer if necessary
        (when (< (buffer-size aggregate)
                 (buffer-size channel))
          (adjust-array (buffer aggregate)
                        (loop for c across channels minimize (buffer-size c))
                        :initial-element 0.0s0))
        ;; Publish
        (setf (channels aggregate) channels))))
  aggregate)
