(in-package #:parendeck2d.graphics)

(deftype color-4 () '(simple-array standard-float (4)))

(declaim (inline col-r))
(defun col-r (c)
  (elt c 0))

(defun (setf col-r) (new-value c)
  (setf (elt c 0) new-value))

(declaim (inline col-g))
(defun col-g (c)
  (elt c 1))

(defun (setf col-g) (new-value c)
  (setf (elt c 1) new-value))

(declaim (inline col-b))
(defun col-b (c)
  (elt c 2))

(defun (setf col-b) (new-value c)
  (setf (elt c 2) new-value))

(declaim (inline col-a))
(defun col-a (c)
  (elt c 3))

(defun (setf col-a) (new-value c)
  (setf (elt c 3) new-value))

(defun make-color-4 (&optional (r +standard-float-zero+) (g +standard-float-zero+) (b +standard-float-zero+) (a +standard-float-zero+))
  (make-array 4 :element-type 'standard-float :initial-contents (list (coerce r 'standard-float)
                                                                      (coerce g 'standard-float)
                                                                      (coerce b 'standard-float)
                                                                      (coerce a 'standard-float))))

(defun lerp-color (v color-a color-b)
  (map 'vector (alexandria:curry #'alexandria:lerp v) color-a color-b))
