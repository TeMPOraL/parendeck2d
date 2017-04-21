(in-package #:parendeck2d.math)

(define-constant +epsilon+ single-float-epsilon :documentation "The difference between 0 and minimum floating point value used in computations.")

(deftype standard-float () 'single-float)

(define-constant +standard-float-zero+ (coerce 0 'standard-float))
(define-constant +pi+ (coerce pi 'standard-float))
(define-constant +2pi+ (coerce (* 2 pi) 'standard-float))

(declaim (ftype (function (float) standard-float) square))
(defun square (x)
  (the standard-float (* x x)))

(define-modify-macro clampf (min max) clamp
  "Modify macro for `CLAMP'. Clamps the value of the place designated by the first argument
to the range [`MIN', `MAX'].")

(defun clamp-vector-elements (vec a b)
  (map 'vector (rcurry #'clamp a b) vec))

(let ((deg/rad (coerce (/ 180 pi) 'standard-float))
      (rad/deg (coerce (/ pi 180) 'standard-float)))
  (declare (ftype (function (standard-float) standard-float)
                  deg->rad rad->deg)
           (inline deg->rad rad->deg))
  (defun deg->rad (deg)
    (declare (optimize (speed 3) (safety 0)))
    (the standard-float (* deg rad/deg)))

  (defun rad->deg (rad)
    (declare (optimize (speed 3) (safety 0)))
    (the standard-float (* rad deg/rad))))

(defun random-float (&optional (min 0.0) (max 1.0) (div 10000.0))
  (let ((random-val (/ (random div) div))
        (range (- max min)))
    (coerce (+ (* random-val range) min) 'standard-float)))
