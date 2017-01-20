(in-package #:parendeck2d.graphics)

(defclass drawable ()
  ())

(defgeneric %draw (drawable)
  (:documentation "Draw the `DRAWABLE'.
All transforms were already set by regular function `DRAW', so the drawable can assume
it's in its local coordinate system."))

(defun draw (drawable &key (x 0.0) (y 0.0) rotation scale-x scale-y transform-offset (position-anchor :center))
  "Draw a `DRAWABLE' at position (`X' `Y'), rotated by `ROTATION' (radians) and scaled by (`SCALE-X' `SCALE-Y').
`TRANSFORM-OFFSET' determines the center for rotation and scaling transformations. It can be any of:
- NIL (default): transform around the point (X, Y)
- (list TX TY): transform around the point (X + TX, X + TY)
`POSITION-ANCHOR' tells how to interpret the (X Y) pair. It can be any of:
- NIL: upper-left corner. (FIXME VERIFY)
- :CENTER - center of drawable (if available), i.e. drawable is drawn AROUND point (X Y).
- ... more values to come.

Transforms are applied in following order:
- translation
- scaling
- rotation"
  (gl:with-pushed-matrix
    (gl:translate x y 0)
    ;; TODO handle position-anchor (additional translate)
    ;; TODO handle transform offset - translate appropriately
    (when rotation
      (gl:rotate rotation 0 0 1))
    (when (or scale-x scale-y)
      (gl:scale (or scale-x 1.0)
                (or scale-y 1.0)
                1.0))
    ;; TODO handle transform offset - untranslate maybe?
    (%draw drawable)))

