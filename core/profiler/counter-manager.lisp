(in-package #:parendeck2d.profiler)

;;; A centralized store for performance counters.


;;; State

(defvar *counters* (make-hash-table) "A hash table storing all existing counters in a (counter-name counter) -> counter mapping.")

(defun register-counter (&key name description interval) ;FIXME history-size
  "Register a new coutner under given `NAME'."
  (setf (gethash name *counters*)
        (make-counter name description interval)))

(defun get-counter (name)
  (let ((counter (gethash name *counters*)))
    (or counter
        (register-counter name "" 0)))  ;FIXME magic?
  )

(defun sample-appropriate-counters (current-time)
  (maphash (lambda (name counter)
             (when (counter-ripe-for-sampling-p counter current-time)
               (sample-counter counter)))
           *counters*))
