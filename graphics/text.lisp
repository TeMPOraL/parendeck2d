(in-package #:parendeck2d.graphics)

(defclass text (drawable)
  ((width :initarg :width
          :reader width)
   (height :initarg :height
           :reader height)))

(defclass rendered-text (text)
  ((text-texture :initarg :texture
                 :reader text-texture)))



(defmethod %draw ((text rendered-text))
  (with-slots (text-texture width height) text
    (bind-texture text-texture)
    (gl:with-primitive :quads           ;FIXME refactor to a generic "draw textured quad" utility
      (gl:tex-coord 1.0 1.0)
      (gl:vertex width 0.0)
      (gl:tex-coord 1.0 0.0)
      (gl:vertex width height)
      (gl:tex-coord 0.0 0.0)
      (gl:vertex 0.0 height)
      (gl:tex-coord 0.0 1.0)
      (gl:vertex 0.0 0.0))))
