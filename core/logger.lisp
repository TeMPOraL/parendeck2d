(in-package #:parendeck2d)

(define-constant +default-log-destination+ "engine.log" :documentation "Default name / destination of the main log file." :test #'string=)

(defun configure-logger ()
  "Sets up log4cl for the engine."
  (uiop/filesystem:delete-file-if-exists +default-log-destination+)  ; a workaround for log4cl apparently being unable to overwrite a log file
  (log:config :daily +default-log-destination+
              :nopretty))
