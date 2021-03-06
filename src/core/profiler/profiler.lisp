(in-package #:parendeck2d.profiler)

;;; A profiling interface integrating performance counters and tracing together into one tool.

;;; TODO
;;; with-profiling -> frame (or second) counter for time / calls (increments) + notes (with metadata) to tracing if enabled
;;; register-event -> frame (or second) counter for event invocations + note (with metadata) to tracing if enabled

(defmacro with-profiling ((counter-name &key (description "") (interval :tick) (history-size +default-counter-history-size+)) &body body)
  "Measure the execution time and count of a block of code using a counter named `COUNTER-NAME'.
Use `INTERVAL' to determine how often a counter should be sampled (in seconds).

Note that using variables for `DESCRIPTION' and `INTERVAL' will have no effect beyond the first runtime invocation
of this macro, as counters currently do not support redefinition."
  (alexandria:with-gensyms (counter now)
    `(let ((,counter (get-counter ,counter-name :description ,description :interval ,interval :history-size ,history-size))
           (,now (p2d:get-current-milliseconds)))
       (prog1
           (progn ,@body)
         (increment-counter ,counter (- (p2d:get-current-milliseconds) ,now))))))

(defmacro with-counter ((counter-symbol counter-name &key (description "") (interval :tick) (history-size +default-counter-history-size+)) &body body)
  "Bind a counter `COUNTER-NAME' to a variable `COUNTER-SYMBOL'.
Basically a shorthand to avoid typing a let."
  `(let ((,counter-symbol (get-counter ,counter-name :description ,description :interval ,interval :history-size ,history-size)))
     ,@body))

(defun count-value (value counter-name &key (description "") (interval :tick) (history-size +default-counter-history-size+))
  "Increment counter `COUNTER-NAME' by `VALUE'.
An utility function to save one from additional nesting level using `WITH-COUNTER'
when a simple increment-by-value is intended."
  (with-counter (counter counter-name :description description :interval interval :history-size history-size)
    (increment-counter counter value)))



#+sbcl
(eval-when (:compile-toplevel :load-toplevel) (require 'sb-sprof))

(defmacro with-statistical-profiling ((&key (file-name "sb-sprof.txt") (max-samples 100000) (mode :cpu)) &body body)
  "Run `BODY' under statistical profiling, if available. Set sample limit to `MAX-SAMPLES'. Save report to `FILE-NAME'.
If `MODE' is set to :CPU, sample CPU time. If set to :ALLOC, sample memory usage."
  #+sbcl(alexandria:with-gensyms
         (graph-report)
         (alexandria:once-only
          (file-name max-samples)
          `(progn
             (sb-sprof:reset)
             (log:info "Starting statistical profiler. Max samples = ~A." ,max-samples)
             (sb-sprof:start-profiling :max-samples ,max-samples :mode ,mode)
             ,@body
             (sb-sprof:stop-profiling)
             (log:info "Saving profiler report to file ~A." ,file-name)
             (with-open-file (,graph-report ,file-name :direction :output :if-exists :supersede)
               (sb-sprof:report :type :graph :stream ,graph-report)))))
  #-sbcl(error "Statistical profiling is not available on this Lisp implementation."))

