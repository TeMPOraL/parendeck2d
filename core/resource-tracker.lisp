(in-package #:parendeck2d)

;;; A simple resource tracker created to count / determine resource leaks.

(defvar *tracked-resources* (tg:make-weak-hash-table :weakness :key :test 'eql))

(defun clear-resource-tracking ()
  (clrhash *tracked-resources*))

(defun track-resource (resource)
  (when-let ((old-timestamp (gethash resource *tracked-resources*)))
    (log:error "Resource already tracked!" resource old-timestamp))
  (setf (gethash resource *tracked-resources*)
        (sdl2:get-ticks))
  resource)

(defun log-tracked-resources-report ()
  (log:debug "~A leaked resources." (hash-table-count *tracked-resources*))
  (let ((now (sdl2:get-ticks)))
    (maphash (lambda (resource timestamp)
               (let ((seconds-alive (float (- now timestamp))))
                (log:debug resource seconds-alive)))
             *tracked-resources*)))
