(in-package #:parendeck2d.graphics)

(defparameter +default-text-size+ 42 "Some default value for text size, meaning whatever.")
(defvar *default-font* nil "Default font to be used if none passed.")

;;; TODO some form of text handling will go here

(defclass font ()
  ((name :initarg :font-name)))


;;; Direct drawing function(s).

(defun draw-text (&key text position (size +default-text-size+) (font *default-font*))
  (declare (ignore text position size font))
  (error "Not yet implemented."))
