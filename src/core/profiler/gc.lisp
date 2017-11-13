(in-package #:parendeck2d.profiler)

(defvar *gc-hook-installed* nil)

(defun install-gc-tracker ()
  "Installs a hook to garbage collector that will ping a counter after GC was invoked."
  (log:info "Installing GC tracker.")
  (setf *gc-hook-installed* t)
  (%install-gc-tracker))

(defun uninstall-gc-tracker ()
  "Clears hook installed by `INSTALL-GC-TRACKER'."
  (log:info "Uninstalling GC tracker.")
  (setf *gc-hook-installed* nil)
  (%uninstall-gc-tracker))

(defun record-gc-used ()
  (count-value 1 'garbage-collector :description "Garbage collector invocations per frame." :interval :frame :history-size 600))


;;; SBCL-specific

#+sbcl
(defun %install-gc-tracker ()
  (push 'record-gc-used sb-ext:*after-gc-hooks*))

#+sbcl
(defun %uninstall-gc-tracker ()
  (alexandria:removef sb-ext:*after-gc-hooks* 'record-gc-used))


;;; CCL-specific

;;; TODO CCL seems to have (undocumented?) after GC hooks feature. Use it.


;;; A portable version.
;;; NOTE The portable version basically creates a throwaway object with finalizer that records GC being invoked,
;;; and spawn a next throwaway object. The idea is to have one such object collected every time GC fires up.
;;; This thus depends on the following assumptions to work:
;;; - that the throwaway object is always finalized by the very next GC invocation
;;;   (violation of this will cause inaccurate stats)
;;; - that the new throwaway object created by previous one's finalizer will NOT be collected in the same GC cycle
;;;   as the previous one
;;;   (violation of this may cause an infinite loop)
;;;
;;; If possible, prefer to use an implementation-specific way if it's well-defined. E.g. after GC hooks, if they're provided.

#-sbcl
(defun %install-gc-tracker ()
  (log:warn "This implementation uses GC tracking that depend on finalizers being called *after* GC finishes collecting objects.")
  (trap-next-gc))

#-sbcl
(defun %uninstall-gc-tracker ()
  ;; empty
  )

#-sbcl
(defun trap-next-gc ()
  (let ((dummy (cons nil nil)))
    (tg:finalize dummy (lambda ()
                         (when *gc-hook-installed*
                           (record-gc-used)
                           (trap-next-gc))))
    (values)))
