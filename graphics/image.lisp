(in-package #:parendeck2d.graphics)

(defclass image (p2d:resource)          ;FIXME best renamed to texture, IMO
  ((width :initarg :width
          :reader width)
   (height :initarg :height
           :reader height)
   (texture-id :initarg :texture-id
               :reader texture-id))

  ;; TODO actual necessary data
  )


;;; 

(defun load-image-from-file (filename)
  (let* ((resource (make-instance 'image
                                  :name filename
                                  :resource-type :image))
         (image-object (sdl2-image:load-image filename))
         (image-width (sdl2:surface-width image-object))
         (image-height (sdl2:surface-height image-object))
         (image-format (sdl2:surface-format image-object))
         (image-pixels (sdl2:surface-pixels image-object))
         (new-texture-id (gl:gen-texture)))
    ;; TODO error handling - SDL problems

    (gl:bind-texture :texture-2d new-texture-id)
    (gl:tex-image-2d :texture-2d 0 :rgba image-width image-height 0 :rgba :unsigned-byte image-pixels) ;FIXME :rgba thing may not be correct
    ;; TODO error handling - OpenGL problems

    (with-slots (width height texture-id p2d:loaded) resource
      (setf width image-width
            height image-height
            texture-id new-texture-id
            p2d:loaded t))
    resource))

(defmethod p2d:unload-resource ((image image))
  (log:debug "Unloading image ~A..." image)
  ;; TODO
  (setf (slot-value image 'p2d:loaded) nil))
