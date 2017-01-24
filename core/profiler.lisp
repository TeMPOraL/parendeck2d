(in-package #:parendeck2d)

;;; A simple inline profiler with output to a format viewable in Google Chrome's chrome://tracing.
;;; Inspired by following articles:
;;; - https://engineering.riotgames.com/news/random-acts-optimization
;;; - http://www.gamasutra.com/view/news/176420/Indepth_Using_Chrometracing_to_view_your_inline_profiling_data.php

(defvar *profiling-events-buffer* nil "A buffer holding all profiling samples.")
(defvar *profiler-running* nil "A flag to tell whether or not profiler is running.")
(defvar *profiling-metadata* nil "Additional metadata to include with profiling report.")


;;; Data format
(defstruct profiling-sample category name processor-id thread-id timestamp phase args)

(defmethod yason:encode ((sample profiling-sample) &optional stream)
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:encode-object-element "cat" (profiling-sample-category sample))
      (yason:encode-object-element "name" (profiling-sample-name sample))
      (yason:encode-object-element "pid" (profiling-sample-processor-id sample))
      (yason:encode-object-element "tid" (profiling-sample-thread-id sample))
      (yason:encode-object-element "ts" (profiling-sample-timestamp sample))
      (yason:encode-object-element "ph" (profiling-sample-phase sample))
      (yason:encode-object-element "args" (profiling-sample-args sample)))))


;;; Profiler management

(defun start-profiling ()
  (unless *profiler-running*
    (reset-profiler-data))
  (setf *profiler-running* t))

(defun stop-profiling ()
  (setf *profiler-running nil))

(defun dump-profiler-data (filename)
  )


;;; Profiling tools for code
(defun add-profiling-sample (category name processor-id thread-id timestamp phase args)
  )



;;; Design notes.
;;; We need as trivial macros to use as possible, which'll get as much data as they can by themselves.
;;; Something like:
;;;
;;; (with-profiling (:name name)
;;;   some code)
;;;
;;; (with-profiling (:data (additional data go here))
;;;   some code)
;;; In the second example, it would be cool to get the name auto-filled.
;;; For that, we can try and directly call #'enclosing-scope-block-name from log4cl.
;;;
;;; Also some macros for instant events:
;;; (profile-instant-event :name name :data data)
;;;
;;; Secondly, it would be awesome to correlate things that go into log file w/ profiling data,
;;; maybe automatically. We can do this in few ways:
;;;
;;; - Have a macro that expands both to profile-instant-event and appropriate log call.
;;; - Write more precise timestamps to log and later correlate it w/ profiler samples in a post-processing step.
;;; - Install a custom log sink that'll convert all log messages into instant events.
;;;
;;; We can treat the first option as fallback. Best option would be (obviously) the third.

