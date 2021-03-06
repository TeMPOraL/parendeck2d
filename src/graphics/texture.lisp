(in-package #:parendeck2d.graphics)


;;; Class definitions

(defclass texture ()
  ((width :initarg :width
          :reader width)
   (height :initarg :height
           :reader height)
   (texture-id :initarg :texture-id
               :reader texture-id)))

(defclass texture-section ()            ;TODO some implementation that would use this.
  ((start-coords :initarg :start
                 :reader start)
   (end-coords :initarg :end
               :reader end)
   (texture :initarg :texture
            :reader texture)))


;;; Texture cache
(defvar *texture-cache* (make-hash-table :test 'equal) "Hashtable mapping texture source filenames to texture objects.")

(defun clear-texture-cache ()
  "Clear all cached textures, freeing them."
  (log:debug "Clearing texture cache." (hash-table-count *texture-cache*))
  (maphash (lambda (name texture)
             (log:debug "Freeing texture ~A - ~A from cache." name texture)
             (%free-texture texture))
           *texture-cache*)
  (clrhash *texture-cache*))

(defun uncache-texture (texture)
  (maphash (lambda (name found-texture)
             (when (eq texture found-texture)
               (log:debug "Removing texture ~A - ~A from cache." name found-texture)
               (remhash name *texture-cache*)))
           *texture-cache*))


;;; Binding and other basic texture utilities.

(defun bind-texture (texture)
  "Bind `TEXTURE' as current in OpenGL."
  ;; TODO fail when (null (texture-id texture)).
  (when (texture-valid-p texture)
    (gl:bind-texture :texture-2d (texture-id texture))))

(defun unbind-current-texture ()
  "Unbind current OpenGL texture."
  (gl:bind-texture :texture-2d 0))

(defun texture-valid-p (texture)
  "T if `TEXTURE' is currently usable with OpenGL, NIL otherwise."
  (numberp (texture-id texture)))


;;; Convenience getters that caches textures.

(defun get-texture (filename)
  "Get texture from `FILENAME'. Uses a cache to avoid loading the same file data multiple times."
  ;; TODO add handling/reporting for (not (texture-valid-p ...)) textures that somehow got stuck in cache.
  (let ((asset-name (p2d:resolve-asset-path filename)))
    (alexandria:if-let ((texture (gethash asset-name *texture-cache*)))
      texture
      (setf (gethash asset-name *texture-cache*)
            (make-texture-from-file asset-name)))))


;;; Creation / deletion functions

(defun make-texture-from-file (filename &key (filter :linear))
  "Read the image in `FILENAME' and turn it into texture."
  ;; TODO handle missing file; we might want to return a "default texture" as well.
  (let ((surface (sdl2-image:load-image filename)))
    (prog1 (make-texture-from-sdl-surface surface :filter filter)
      (sdl2:free-surface surface))))

(defun make-texture-from-sdl-surface (surface &key (filter :linear) (wrap nil))
  "Convert data from `SURFACE' into a texture. Does NOT free the surface."
  (let ((image-width (sdl2:surface-width surface))
        (image-height (sdl2:surface-height surface))
        (new-texture-id (gl:gen-texture)))

    (gl:bind-texture :texture-2d new-texture-id)

    ;; FIXME calculate image format data properly for maximum portability.

    ;; HACK now this is just lazy cheating.
    ;; Replace with optional conversion if/when performance or memory becomes a concern,
    ;; or you get mighty lazy.
    (let ((converted-surface (sdl2:convert-surface-format surface :abgr8888)))
      (gl:tex-image-2d :texture-2d 0 :rgba image-width image-height 0 :rgba :unsigned-byte (sdl2:surface-pixels converted-surface))
      (sdl2:free-surface converted-surface))
    
    ;; (gl:tex-image-2d :texture-2d 0 :rgba image-width image-height 0 :rgba :unsigned-byte image-pixels)

    ;; TODO maybe fully parametrize min/mag filters, and also wraps for creation.
    (gl:tex-parameter :texture-2d :texture-min-filter filter)
    (gl:tex-parameter :texture-2d :texture-mag-filter filter)

    (unless wrap
      (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
      (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge))

    ;; TODO error handling - OpenGL problems

    (make-instance 'texture
                   :width image-width
                   :height image-height
                   :texture-id new-texture-id)))

(defun make-blank-texture (width height)
  "Creates a blank RGBA texture of size `WIDTH' x `HEIGHT'."
  (declare (ignore width height))
  (error "Not yet implemented.")
  ;; TODO to make a blank texture, pass a null pointer to #'gl:tex-image-2d.
  )

(defun free-texture (texture)
  "Removes `TEXTURE' from OpenGL. It's no longer valid to use (its `TEXTURE-ID' may be reused)."
  (log:trace "Freeing texture ~A." texture)
  (uncache-texture texture)
  (%free-texture texture))

(defun %free-texture (texture)
  (when (texture-id texture)
    (gl:delete-textures (list (texture-id texture)))
    (setf (slot-value texture 'texture-id) nil)))


;;; Render-to-texture
;;; TODO at some point in the future.


;;; Utilities
(defmacro with-texture (texture &body body)
  "Temporarily bind `TEXTURE', execute `BODY', then restore previously bound texture."
  (alexandria:with-gensyms (old-texture)
    `(let ((,old-texture (gl:get-integer :texture-binding-2d)))
       (bind-texture ,texture)
       (progn ,@body)
       (gl:bind-texture :texture-2d ,old-texture))))


;;; Printers

(defmethod print-object ((texture texture) stream)
  (print-unreadable-object (texture stream :type t :identity t)
    (format stream "id: ~A; ~A x ~A" (texture-id texture) (width texture) (height texture))))

(defmethod print-object ((texture-section texture-section) stream)
  (print-unreadable-object (texture-section stream :type t :identity t)
    (with-slots (start-coords
                 end-coords
                 texture)
        texture-section
      (format stream "of texture ~A, ~A -> ~A"
              (if texture
                  (concatenate 'string "#"(texture-id texture))
                  "N/A")
              start-coords
              end-coords))))
