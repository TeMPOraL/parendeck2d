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
        (error (log:fatal "Lisp error: ~A." condition))
        (warning (log:warn "Lisp warning: ~A." condition))
        (t (log:fatal "Lisp condition: ~A" condition))))))

(defmacro with-logging-conditions (&body forms)
  "Set up a passthrough condition handler that will log all signalled conditions."
  `(handler-bind ((t #'log-condition))
     ,@forms))
