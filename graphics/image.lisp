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

    (log:debug image-format)
    ;; TODO error handling - SDL problems

    ;; file open problem: SDL Error
    ;; <FATAL> [13:32:29] p2d logger.lisp (log-condition) - Lisp condition: SDL Error (#<SDL-SURFACE {#X00000000}>): Couldn't open /home/temporal/repos/lisp-games/tswr-asteroids/trc.png
    ;; 4: (UIOP/IMAGE:PRINT-CONDITION-BACKTRACE #<SDL2-IMAGE:SDL-IMAGE-ERROR {1002EA6F03}> :STREAM #<SB-IMPL::STRING-OUTPUT-STREAM {1002EAA923}> :COUNT NIL)
    ;; If there's a problem loading image, we'd like to log it / signal it somehow, but return a default image just in case.

    (gl:bind-texture :texture-2d new-texture-id)
    (gl:tex-image-2d :texture-2d 0 :rgba image-width image-height 0 :rgba :unsigned-byte image-pixels) ;FIXME :rgba thing may not be correct
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)

    ;; TODO error handling - OpenGL problems

    (log:debug (gl:get-error))

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
