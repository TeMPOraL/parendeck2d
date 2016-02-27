(in-package #:parendeck2d.gl-utils)

(declaim (inline translate2))
(defun translate2 (vec)
  (gl:translate (vec-x vec)
                (vec-y vec)
                0.0))

(declaim (inline translate3))
(defun translate3 (vec)
  (gl:translate (vec-x vec)
                (vec-y vec)
                (vec-z vec)))

(declaim (inline scale2-uniform))
(defun scale2-uniform (scale)
  (gl:scale scale scale 1))

(declaim (inline scale3-uniform))
(defun scale3-uniform (scale)
  (gl:scale scale scale scale))

(declaim (inline color4))
(defun color4 (color)
  (gl:color (col-r color)
            (col-g color)
            (col-b color)
            (col-a color)))
