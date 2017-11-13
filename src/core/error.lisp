(in-package #:parendeck2d)

;;; Error policy controls.

(deftype error-policy '()
  "Error policy enum."
  '(member :silet :fail-hard))

(defparameter *error-policy* :silent)

