(in-package #:parendeck2d)

;;; printers for various third-party objects - useful for logging and interactive work
(defmethod print-object ((key sdl2-ffi:sdl-keysym) stream)
  (print-unreadable-object (key stream :type t :identity t)
    (format stream "key: ~A (~A)" (sdl2:scancode key) (sdl2:scancode-value key))))
