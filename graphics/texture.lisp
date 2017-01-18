(in-package #:parendeck2d.graphics)


;;; Class definitions

(defclass texture ()
  ((width :initarg :width
          :reader width)
   (height :initarg :height
           :reader height)
   (texture-id :initarg :texture-id
               :reader texture-id)))

(defclass texture-section ()
  ((start-coords :initarg :start
                 :reader start)
   (end-coords :initarg :end
               :reader end)
   (texture :initarg :texture
            :reader texture)))


;;; Texture cache
(defvar *texture-cache* (make-hash-table :test 'equal))

(defun clear-image-cache ()
  (maphash (lambda (name texture)
             (log:debug "Freeing texture ~A - ~A from cache." name texture)
             (free-texture texture))
           *texture-cache*)
  (clrhash *image-cache*))


;;; Convenience getters that caches textures.
(defun get-texture (filename)
  "Get texture from `FILENAME'. Uses a cache to avoid loading the same file data multiple times."
  (alexandria:if-let ((texture (gethash filename *texture-cache*)))
    texture
    (setf (gethash filename *texture-cache*)
          (make-texture-from-file filename))))


;;; Creation / deletion functions

(defun make-texture-from-file (filename)
  "Read the image in `FILENAME' and turn it into texture."
  ;; TODO handle missing file
  (make-texture-from-sdl-surface (sdl2-image:load-image filename)))

(defun make-texture-from-sdl-surface (surface)
  "Convert data from `SURFACE' into a texture. Does NOT free the surface."
  (let ((image-width (sdl2:surface-width surface))
        (image-height (sdl2:surface-height surface))
        (image-format (sdl2:surface-format surface))
        (image-pixels (sdl2:surface-pixels surface))
        (new-texture-id (gl:gen-texture)))

    (gl:bind-texture :texture-2d new-texture-id)

    ;; FIXME calculate image format data properly for maximum portability.
    (gl:tex-image-2d :texture-2d 0 :rgba image-width image-height 0 :rgba :unsigned-byte image-pixels)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)

    ;; TODO error handling - OpenGL problems

    (make-instance 'texture
                   :width image-width
                   :height image-height
                   :texture-id new-texture-id)))

(defun make-blank-texture (width height)
  "Creates a blank RGBA texture of size `WIDTH' x `HEIGHT'."
  (error "Not yet implemented."))

(defun free-texture (texture)
  "Removes `TEXTURE' from OpenGL. It's no longer valid to use (its `TEXTURE-ID' may be reused)."
  (when (texture-id texture)
    (gl:delete-textures (list (texture-id texture)))))


;;; Printers

(defmethod print-object ((texture texture) stream)
  (print-unreadable-object (texture stream :type t :identity t)
    (format stream "id: ~A; ~A x ~A" (texture-id texture) (width texture) (height texture))))

(defmethod print-object ((texture-section texture-section) stream)
  (print-unreadable-object (texture-section stream :type t :identity t)
    (with-slots (start-coords
                 end-coords
                 texture)
        instance
      (format stream "of texture ~A, ~A -> ~A"
              (if texture
                  (concatenate 'string "#"(texture-id texture))
                  "N/A")
              start-coords
              end-coords))))
