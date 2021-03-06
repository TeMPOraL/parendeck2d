(in-package #:parendeck2d.math)

(deftype vector-2d () '(simple-array standard-float (2)))
(deftype vector-3d () '(simple-array standard-float (3)))
(deftype vector-4d () '(simple-array standard-float (4)))

(deftype vector-Nd () '(simple-array standard-float (*)))

(defun vec-x (a)
  (elt a 0))

(defun (setf vec-x) (new-value a)
  (setf (elt a 0) new-value))

(defun vec-y (a)
  (elt a 1))

(defun (setf vec-y) (new-value a)
  (setf (elt a 1) new-value))

(defun vec-z (a)
  (elt a 2))

(defun (setf vec-z) (new-value a)
  (setf (elt a 2) new-value))

(defun vec-w (a)
  (elt a 3))

(defun (setf vec-w) (new-value a)
  (setf (elt a 3) new-value))

(declaim (ftype (function (&optional standard-float standard-float) vector-2d) make-vector-2d))
(defun make-vector-2d (&optional (a +standard-float-zero+) (b +standard-float-zero+))
   "Creates a new 2D vector."
   (make-array 2 :element-type 'standard-float :initial-contents (list a b)))

(defun make-vector-3d (&optional (a +standard-float-zero+) (b +standard-float-zero+) (c +standard-float-zero+))
  "Creates a new 3D vector."
  (make-array 3 :element-type 'standard-float :initial-contents (list (coerce a 'standard-float) (coerce b 'standard-float) (coerce c 'standard-float))))

(defun make-vector-4d (&optional (a +standard-float-zero+) (b +standard-float-zero+) (c +standard-float-zero+) (d +standard-float-zero+))
  "Creates a new 3D vector."
  (make-array 4 :element-type 'standard-float :initial-contents (list (coerce a 'standard-float) (coerce b 'standard-float) (coerce c 'standard-float) (coerce d 'standard-float))))

(defun vector-2d-p (vec)
  "Checks if passed sequence is a 2D vector."
  (= (length vec) 2))

(defun vector-3d-p (vec)
  "Checks if passed sequence is a 3D vector."
  (= (length vec) 3))

(defun vector-4d-p (vec)
  "Checks if passed sequence is a 4D vector."
  (= (length vec) 4))

(defun add-vectors (vec1 vec2)
  "Sums two vectors."
  (map 'vector #'+ vec1 vec2))

(defun add-to-vector (vec1 vec2)
  "Sums two vectors into the first one."
  (map-into vec1 #'+ vec1 vec2))

(defun subtract-vectors (vec1 vec2)
  (map 'vector #'- vec1 vec2))

(defun subtract-from-vector (vec1 vec2)
  (map-into vec1 #'- vec1 vec2))

(defun scaled-vector (vec1 scale)
  "Returns a new vector - vec1 scaled by given scalar."
  (map 'vector (lambda (elem) (* elem scale)) vec1))

(defun scale-vector (vec1 scale)
  "Scales vector by given scalar."
  (map-into vec1 (lambda (elem) (* elem scale)) vec1))

(defun negative-vector (vec1)
  "Returns a new vector - negation of given one."
  (map 'vector #'- vec1))

(defun negate-vector (vec1)
  "Negates current vector."
  (map-into vec1 #'- vec1))

(defun distance-between-vectors-squared (vec1 vec2)
  (loop for x across vec1
     for y across vec2
     summing (square (- x y))))

(defun distance-between-vectors (vec1 vec2)
  (sqrt (distance-between-vectors-squared vec1 vec2)))

(defun vector-value-squared (vec1)
  (reduce (lambda (total x) (+ total (square x))) vec1 :initial-value 0.0))

(defun vector-value (vec1)
  (sqrt (vector-value-squared vec1)))

(defun (setf vector-value) (new-val vec)
  (scale-vector vec (/ new-val (vector-value vec)))) ;NOTE division by 0 (possibly)

(defun normalized-vector (vec1)
  (let ((len (vector-value vec1)))
    (if (< (abs len) +epsilon+)
        (scaled-vector vec1 0)
        (scaled-vector vec1 (/ 1.0 len)))))

(defun vector-of-length (vec1 len)
  (scaled-vector (normalized-vector vec1) len)) ;NOTE division by 0 (possibly), also DRY implementation with (setf vector-value)

(defun clamp-vector (vec1 max-length)
  (let ((length (vector-value vec1)))
    (when (> length max-length)
      (scale-vector vec1 (/ max-length length)))))

(defun clamped-vector (vec1 max-length)
  (let ((length (vector-value vec1)))
    (if (> length max-length)
        (scaled-vector vec1 (/ max-length length))
        (scaled-vector vec1 1.0)        ;FIXME make a separate "copy vector" function
        )))

(defun reflect-vector (vec normal)
  "Reflect `VEC' from surface given by `NORMAL'. Modifies the vector."
  (subtract-from-vector vec
                        (scaled-vector normal (* 2 (⋅ vec normal)))))

(defun reflected-vector (vec normal)      ;FIXME reflect_ed_ - name convention?
  "Reflect `VEC' from surface given by `NORMAL'."
  (subtract-vectors vec
                    (scaled-vector normal (* 2 (⋅ vec normal)))))

(defun rotate-vector-2d (vec theta)
  "Rotates `VEC'by `THETA', CCW."
  (let ((cos (cos theta))
        (sin (sin theta)))
    (psetf (vec-x vec) (- (* (vec-x vec) cos)
                          (* (vec-y vec) sin))
           (vec-y vec) (+ (* (vec-x vec) sin)
                          (* (vec-y vec) cos)))))

(defun rotated-vector-2d (vec theta)
  "Returns a copy of `VEC' rotated by `THETA', CCW."
  (let ((cos (cos theta))
        (sin (sin theta)))
    (make-vector-2d (- (* (vec-x vec) cos)
                       (* (vec-y vec) sin))
                    (+ (* (vec-x vec) sin)
                       (* (vec-y vec) cos)))))

(defun vector-angle-2d (vec2d)          ;FIXME maybe better name?
  "Returns the angle between `VEC2D' and the X axis."
  (atan (vec-y vec2d) (vec-x vec2d)))

;;; TEST

(defun reflected-vector-test ()
  (let ((v1 (make-vector-4d 2 0 0))
        (v2 (make-vector-4d 2 2 0))
        (n1 (make-vector-4d 0 1 0))
        (n2 (normalized-vector (make-vector-4d 1 1 0))))
    (list (reflected-vector v1 n1)
          (reflected-vector v2 n1)
          (reflected-vector v1 n2))))

;;; /TEST

#+nil(declaim (ftype (function (vector-Nd vector-Nd) standard-float) ⋅))
(defun ⋅ (a b)
  (declare (optimize (speed 3)))
  "Compute dot product of two vectors."
  (the standard-float (reduce #'+ (map 'vector #'* a b))))

(defparameter dot-product '())
(setf (symbol-function 'dot-product) #'⋅)

(defun × (a b)
  "Compute cross product of two 3D vectors."
  (make-vector-3d (- (* (vec-y a) (vec-z b))
                     (* (vec-y b) (vec-z a)))
                  (- (* (vec-z a) (vec-x b))
                     (* (vec-z b) (vec-x a)))
                  (- (* (vec-x a) (vec-y b))
                     (* (vec-x b) (vec-y a)))))

(defparameter cross-product '())
(setf (symbol-function 'cross-product) #'×)
