(in-package #:parendeck2d.profiler)

;;; A profiling interface integrating performance counters and tracing together into one tool.

;;; TODO
;;; with-profiling -> frame (or second) counter for time / calls (increments) + notes (with metadata) to tracing if enabled
;;; register-event -> frame (or second) counter for event invocations + note (with metadata) to tracing if enabled

(defmacro with-profiling ((counter-name &key (description "") (interval 0)) &body body)
  "Measure the execution time and count of a block of code using a counter named `COUNTER-NAME'.
Use `INTERVAL' to determine how often a counter should be sampled (in seconds).

Note that using variables for `DESCRIPTION' and `INTERVAL' will have no effect beyond the first runtime invocation
of this macro, as counters currently do not support redefinition."
  (alexandria:with-gensyms (counter now)
    `(let ((,counter (get-counter ,counter-name :description ,description :interval ,interval))
           (,now (p2d:get-current-milliseconds)))
       (prog1
           (progn ,@body)
         (increment-counter ,counter (- (p2d:get-current-milliseconds) ,now))))))

(defmacro with-counter ((counter-symbol counter-name &key (description "") (interval 0)) &body body)
  "Bind a counter `COUNTER-NAME' to a variable `COUNTER-SYMBOL'.
Basically a shorthand to avoid typing a let."
  `(let ((,counter-symbol (get-counter ,counter-name :description ,description :interval ,interval)))
     ,@body))
