(in-package #:parendeck2d.profiler)

;;; A centralized store for performance counters.


;;; State

(defvar *counters* (make-hash-table) "A hash table storing all existing counters in a (counter-name counter) -> counter mapping.")

(defun register-counter (&key name description interval) ;FIXME history-size
  "Register a new coutner under given `NAME'."
  (setf (gethash name *counters*)
        (make-counter name description interval)))

(defun get-counter (name &key (description "") (interval 0))
  (let ((counter (gethash name *counters*)))
    (or counter
        (register-counter :name name :description description :interval interval))))

(defun sample-appropriate-counters (current-time)
  (maphash (lambda (name counter)
             (declare (ignore name))
             (when (counter-ripe-for-sampling-p counter current-time)
               (sample-counter counter current-time)))
           *counters*))

(defun clear-all-counters ()
  "Removes all managed counters."
  (clrhash *counters*))


;;; Counter reporter

(defun write-counter-report (filename)
  "Writes current state of all counters to a report file named `FILENAME'."
  (with-open-file (file filename :direction :output :if-exists :supersede)
    (who:with-html-output (file)
      (:html
       (write-report-header file)
       (:body
        (maphash (lambda (name counter)
                   (declare (ignore name))
                   (write-counter-details file counter))
                 *counters*))))))

(defun write-report-header (stream)
  (who:with-html-output (stream)
    (:head
     (:meta :charset "UTF-8")
     (:title "P2D performance counters report")
     ;; TODO helper styles and JS
     )))

(defun write-counter-details (stream counter)
  (who:with-html-output (stream)
    (:div :class "counter-report"
          (:h2 (who:str (counter-name counter)))
          (:p (who:esc (counter-description counter)))
          (:ul
           (:li "Buffer size: TODO")
           (:li (who:fmt "Sampling every ~A second~:P" (counter-sampling-interval counter)))
           (:li "Last recorded value"
                (:ul (:li "Increments: " (who:str (counter-current-increments counter)))
                     (:li "Sample: " (who:str (counter-current-sample counter)))))
           (:li "Stats"
                (:ul (:li (who:fmt "Increments (min/running avg/max): ~A/~A/~A"
                                   (coerce (counter-increments-global-min counter) 'double-float)
                                   (coerce (counter-increments-running-avg counter) 'double-float)
                                   (coerce (counter-increments-global-max counter) 'double-float)))
                     (:li (who:fmt "Samples (min/running avg/max): ~A/~A/~A"
                                   (coerce (counter-samples-global-min counter) 'double-float)
                                   (coerce (counter-samples-running-avg counter) 'double-float)
                                   (coerce (counter-samples-global-max counter) 'double-float))))))
          ;; TODO bulk of data as charts w/ clickable values
          ;; TODO bulk of data as exportable data
          )))
