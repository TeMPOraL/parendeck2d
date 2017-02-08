(in-package #:parendeck2d.profiler)

;;; A simple inline profiler with output to a format viewable in Google Chrome's chrome://tracing.
;;; Inspired by following articles:
;;; - https://engineering.riotgames.com/news/random-acts-optimization
;;; - http://www.gamasutra.com/view/news/176420/Indepth_Using_Chrometracing_to_view_your_inline_profiling_data.php

(defvar *tracing-events-buffer* nil "A buffer holding all profiling samples.")
(defvar *tracing-running* nil "A flag to tell whether or not profiler is running.")
(defvar *tracing-metadata* nil "Additional metadata to include with profiling report.")


;;; Data format
;;; FIXME redo as defclass or whatevs.
(defstruct tracing-sample category name processor-id thread-id timestamp phase args)

(defmethod yason:encode ((sample tracing-sample) &optional stream)
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:encode-object-element "cat" (tracing-sample-category sample))
      (yason:encode-object-element "name" (tracing-sample-name sample))
      (yason:encode-object-element "pid" (tracing-sample-processor-id sample))
      (yason:encode-object-element "tid" (tracing-sample-thread-id sample))
      (yason:encode-object-element "ts" (tracing-sample-timestamp sample))
      (yason:encode-object-element "ph" (tracing-sample-phase sample))
      (yason:encode-object-element "args" (tracing-sample-args sample)))))

