(in-package #:parendeck2d)

(define-constant +epsilon+ single-float-epsilon :documentation "The difference between 0 and minimum floating point value used in computations.")

(deftype standard-float () 'single-float)

(define-constant +standard-float-zero+ (coerce 0 'standard-float))

(declaim (ftype (function (float) standard-float) square))
(defun square (x)
  (the standard-float (* x x)))

;; (defun clamp (what a b)
;;   (if (< what a) a
;;       (if (> what b) b
;;           what)))

(defun clamp-vector-elements (vec a b)
  (map 'vector (rcurry #'clamp a b) vec))

(let ((deg/rad (/ 180 pi))
      (rad/deg (/ pi 180)))
  (declare (ftype (function (standard-float) standard-float)
                  deg->rad rad->deg)
           (inline deg->rad rad->deg))
  (defun deg->rad (deg)
    (declare (optimize (speed 3) (safety 0)))
    (the standard-float (* deg rad/deg)))

  (defun rad->deg (rad)
    (declare (optimize (speed 3) (safety 0)))
    (the standard-float (* rad deg/rad))))
