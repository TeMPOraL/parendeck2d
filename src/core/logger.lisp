(in-package #:parendeck2d)

(define-constant +default-log-destination+ "engine.log" :documentation "Default name / destination of the main log file." :test #'string=)
(defparameter *logging-condition* nil)

(defun configure-logger ()
  "Sets up log4cl for the engine."
  (uiop/filesystem:delete-file-if-exists +default-log-destination+)  ; a workaround for log4cl apparently being unable to overwrite a log file
  (log:config :daily +default-log-destination+
              :nopretty
              :immediate-flush))

(defun log-condition (condition)
  (unless *logging-condition*
    (let ((*logging-condition* t))
      (typecase condition
        (warning (log:warn "Lisp warning: ~A." condition))
        (t (let ((backtrace (with-output-to-string (s)
                              (uiop/image:print-condition-backtrace condition :stream s))))
             (log:fatal "Lisp condition: ~A" condition)
             (log:fatal "~A" backtrace)))))))

(defun call-with-logging-conditions (func)
  (handler-bind ((t #'log-condition))
    (funcall func)))

(defmacro with-logging-conditions (&body forms)
  "Set up a passthrough condition handler that will log all signalled conditions."
  `(call-with-logging-conditions (lambda () ,@forms)))

(defmacro ignore-and-log-errors (&body forms)
  "A replacement for `IGNORE-ERRORS' that also logs the condition.
Log entry will display the location in which this macro was expanded."
  `(handler-case
       (progn ,@forms)
     (t (condition) (progn
                      (log:warn "Caught and ignored a condition." condition)))))

