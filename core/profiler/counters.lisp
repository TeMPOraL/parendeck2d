(in-package #:parendeck2d.profiler)

;;;; A low-level performance counters implementation.
;;;; Implements a stand-alone performance counter; meant as a base
;;;; for a system managing those counters.


;;; A ring buffer.
(defparameter +default-counter-samples-ring-buffer-size+ 32)

(defclass counter-samples-ring-buffer ()
  ((store :type vector)
   (write :type fixnum
          :initform 0)
   (size :type fixnum
         :reader csrb-size)))

(defmethod initialize-instance :after ((buffer counter-samples-ring-buffer) &rest initargs &key (size +default-counter-samples-ring-buffer-size+))
  (declare (ignore initargs)
           (fixnum size))
  (setf (slot-value buffer 'store) (make-array size)
        (slot-value buffer 'size) size))

(defun csrb-push-value (buffer value)
  (declare (optimize (speed 3)))
  (with-slots (store write size)
      buffer
    (declare (type fixnum write size)
             (type vector store))
    (let ((oldest-value (aref store write)))
      (setf (aref store write) value
            write (mod (1+ write) size))
      oldest-value)))

(defun csrb-list-values (buffer)
  (declare (optimize (speed 3)))
  (let ((store (slot-value buffer 'store))
        (write (slot-value buffer 'write))
        (size (slot-value buffer 'size)))
    (declare (type fixnum write size))
    (loop repeat size
       for i = write then (mod (1+ i) size)
       collect (aref store i))))

(defun csrb-values (buffer)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let ((store (slot-value buffer 'store))
        (write (slot-value buffer 'write))
        (size (slot-value buffer 'size)))
    (declare (type fixnum write size))
    (let ((result (make-array size)))
      (dotimes (i size)
        (setf (aref result i)
              (aref store (the fixnum (mod (+ write i) size)))))
      result)))

(defmethod print-object ((buffer counter-samples-ring-buffer) stream)
  (print-unreadable-object (buffer stream :type t :identity t)
    (with-slots (size store)
        buffer
      (format stream "~A: ~A" size store))))


;;; A performance counter.
(defclass counter ()
  ((name :initarg :name
         :reader counter-name)
   (description :initarg :description
                :reader counter-description)
   
   (sampling-interval :initarg :sampling-interval
                      :reader counter-sampling-interval)

   (current-sample :reader counter-current-sample
                   :initform 0)
   (current-increments :reader counter-current-increments
                       :initform 0)
   (last-sampling-time :reader counter-last-sampling-time
                       :initform 0)

   
   (last-n-samples :reader counter-last-n-samples)
   (samples-global-min :reader counter-samples-global-min
                       :initform nil)
   (samples-global-max :reader counter-samples-global-max
                       :initform nil)
   (samples-running-avg :reader counter-samples-running-avg
                        :initform 0)
   
   (last-n-increments :reader counter-last-n-increments)
   (increments-global-min :reader counter-increments-global-min
                          :initform nil)
   (increments-global-max :reader counter-increments-global-max
                          :initform nil)
   (increments-running-avg :reader counter-increments-running-avg
                           :initform 0)))

(defmethod initialize-instance :after ((counter counter) &rest initargs &key history-size)
  (declare (ignore initargs))
  (setf (slot-value counter 'last-n-samples) (make-instance 'counter-samples-ring-buffer :size (coerce history-size 'fixnum))
        (slot-value counter 'last-n-increments) (make-instance 'counter-samples-ring-buffer :size (coerce history-size 'fixnum))))

(defun make-counter (name desc interval) ;FIXME maybe parametrized size instead of hardcoded?
  (make-instance 'counter
                 :name name
                 :description desc
                 :sampling-interval interval
                 :history-size +default-counter-samples-ring-buffer-size+))

(defun increment-counter (counter &optional (value 1.0))
  (with-slots (current-sample
               current-increments)
      counter
    (incf current-sample value)
    (incf current-increments)))

(defun sample-counter (counter current-time)
  (with-slots (current-sample
               samples-global-min
               samples-global-max
               last-n-samples
               samples-running-avg)
      counter
    ;; handle sample value
    ;; - adjust global min/max
    (when (or (null samples-global-min)
              (< current-sample samples-global-min))
      (setf samples-global-min current-sample))
    (when (or (null samples-global-max)
              (> current-sample samples-global-max))
      (setf samples-global-max current-sample))
    
    ;; - push value into buffer
    (let ((oldest-sample (csrb-push-value last-n-samples current-sample)))
      ;; - adjust running avg
      (incf samples-running-avg (+ (/ current-sample (csrb-size last-n-samples))
                                   (- (/ oldest-sample (csrb-size last-n-samples))))))
    ;; - clear current sample
    (setf current-sample 0)
    )

  ;; handle sample increments

  (with-slots (current-increments
               increments-global-min
               increments-global-max
               last-n-increments
               increments-running-avg)
      counter
    ;; - adjust global min/max
    (when (or (null increments-global-min)
              (< current-increments increments-global-min))
      (setf increments-global-min current-increments))
    (when (or (null increments-global-max)
              (> current-increments increments-global-max))
      (setf increments-global-max current-increments))

    ;; - push value into buffer
    (let ((oldest-increment (csrb-push-value last-n-increments current-increments)))
      ;; - adjust running avg
      (incf increments-running-avg (+ (/ current-increments (csrb-size last-n-increments))
                                      (- (/ oldest-increment (csrb-size last-n-increments))))))

    ;; - clear current increments
    (setf current-increments 0))

  ;; set last sampling time to appropriate value

  (setf (slot-value counter 'last-sampling-time) current-time))

(defun counter-ripe-for-sampling-p (counter current-time)
  (> (- current-time
        (slot-value counter 'last-sampling-time))
     (slot-value counter 'sampling-interval)))

(defmethod print-object ((counter counter) stream)
  (print-unreadable-object (counter stream :type t :identity t)
    (with-slots (name description sampling-interval)
        counter
      (format stream "~A (~A) sampled every ~A second~:p" name description sampling-interval))))
