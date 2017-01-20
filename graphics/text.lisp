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
    (draw-rectangle 0 0 width height)
    (unbind-current-texture)))
