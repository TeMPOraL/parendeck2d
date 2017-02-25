(in-package #:parendeck2d.profiler)

(defvar *gc-hook-installed* nil)

(defun install-gc-tracker ()
  "Installs a hook to garbage collector that will ping a counter after GC was invoked."
  (log:info "Installing GC tracker.")
  (setf *gc-hook-installed* t)
  (trap-next-gc))

(defun uninstall-gc-tracker ()
  "Clears hook installed by `INSTALL-GC-TRACKER'."
  (log:info "Uninstalling GC tracker.")
  (setf *gc-hook-installed* nil))



;;; A portable version.
(defun trap-next-gc ()
  (let ((dummy (cons nil nil)))
    (tg:finalize dummy (lambda ()
                         (when *gc-hook-installed*
                           (count-value 1 'garbage-collector :description "Garbage collector invocations per frame." :interval :frame :history-size 600)
                           (trap-next-gc))))
    (values)))
