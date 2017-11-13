(in-package #:parendeck2d)

;;; A simple resource tracker created to count / determine resource leaks.

(defvar *tracked-resources* (tg:make-weak-hash-table :weakness :key :test 'eql))

(defun clear-resource-tracking ()
  (clrhash *tracked-resources*))

(defun track-resource (resource)
  (when-let ((old-timestamp (gethash resource *tracked-resources*)))
    (log:error "Resource already tracked!" resource old-timestamp))
  (setf (gethash resource *tracked-resources*)
        (get-current-milliseconds))
  resource)

(defun log-tracked-resources-report ()
  (log:debug "~A leaked resources." (hash-table-count *tracked-resources*))
  (let ((now (get-current-milliseconds)))
    (maphash (lambda (resource timestamp)
               (let ((seconds-alive (msec-delta-in-seconds timestamp now)))
                (log:debug resource seconds-alive)))
             *tracked-resources*)))
