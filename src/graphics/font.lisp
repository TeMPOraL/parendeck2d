(in-package #:parendeck2d.graphics)

(defparameter *default-font-size* 42 "Font size to use for creating rendered fonts. In sorta-pixels.")
(defvar *font-cache* (make-hash-table :test 'equal) "Hashtable mapping font source filenames to font objects.")


;;; Base class

(defclass font ()
  ())

(defgeneric %free-font (font)
  (:documentation "Free all `FONT' resources."))

(defgeneric font-valid-p (font)
  (:documentation "Checks if `FONT' is valid and can be used to render text."))

(defgeneric render-text (font text)
  (:documentation "Renders `TEXT' using `FONT' and returns an object that can be later drawn on the screen."))


;;; Rendered fonts - ones that draw text by rendering it to texture.

(defclass rendered-font (font)
  ((internal-font-object :initarg :internal-font-object
                         :reader internal-font-object)
   (font-name :initarg :font-name
              :reader font-name)
   (font-size :initarg :font-size
              :reader font-size)))

(defmethod %free-font ((font rendered-font))
  (sdl2-ttf:close-font (internal-font-object font))
  (setf (slot-value font 'internal-font-object) nil))

(defmethod font-valid-p ((font rendered-font))
  (not (null (internal-font-object font))))

(defmethod render-text ((font rendered-font) text)
  (let* ((surface (sdl2-ttf:render-text-blended (internal-font-object font) text 255 255 255 255))
         (texture (make-texture-from-sdl-surface surface)))
    (when surface
      ;; cl-sdl2-ttf sets up a finalizer that auto-frees rendered surface when it's garbage collected.
      ;; We don't want for that data to hang out in memory for that long, so we'll free it ourselves.
      (tg:cancel-finalization surface)
      (sdl2:free-surface surface))
    (when texture
      (p2d:track-resource (make-instance 'rendered-text :width (width texture) :height (height texture) :texture texture)))))


;;; Bitmap fonts - ones in which text is assembled from textured quads based on a glyph tilemap.

(defclass bitmap-font (font)
  ()
  ;; TODO
  )


;;; Font cache.
;;; FIXME pretty much identical to texture cache. DRY the cache code.
;;;
;;; FIXME FIXME RENDERED FONTS ARE PROBABLY DEFINED BY THEIR PT SIZE ON LOAD!!
;;; May need to do separate caching for rendered and bitmap fonts, and use
;;; name + size as a key for rendered font cache.
;;;
;;; TODO Since we don't plan on having very many fonts, we may as well make the cache
;;; a list, or something.

(defun clear-font-cache ()
  "Clear all cached fonts, freeing them."
  (log:debug "Clearing font cache." (hash-table-count *font-cache*))
  (maphash (lambda (name font)
             (log:debug "Freeing font ~A - ~A from cache." name font)
             (%free-font font))
           *font-cache*)
  (clrhash *font-cache*))

(defun uncache-font (font)
  (maphash (lambda (name found-font)
             (when (eq font found-font)
               (log:debug "Removing font ~A - ~A from cache." name found-font)
               (remhash name *font-cache*)))
           *font-cache*))


;;; Convenience getters.
(defun get-rendered-font (filename &key (size *default-font-size*))
  "Get a rendered font from `FILENAME'. Uses a cache to avoid loading and storing the same font data multiple times."
  ;; FIXME add handling/reporting if:
  ;; - an invalid (e.g. freed) font got somehow stuck in cache
  ;; - cached font is not a RENDERED-FONT

  ;; TODO font cache should distinguish between same rendered fonts of different size
  (let ((asset-path (p2d:resolve-asset-path filename)))
    (alexandria:if-let ((font (gethash asset-path *font-cache*)))
      font
      (setf (gethash asset-path *font-cache*)
            (make-rendered-font-from-file asset-path size)))))

(defun get-bitmap-font (filename)
  "Get a bitmap font from `FILENAME'. Uses a cache to avoid loading and storing the same font data multiple times."
  (declare (ignore filename))
  (error "Not yet implemented."))


;;; Creation / deletion.
(defun make-rendered-font-from-file (filename size)
  "Reads the font file `FILENAME' and creates a `RENDERED-FONT' object."
  ;; TODO maybe we need a pixels -> points conversion based on current DPI?
  (let ((font-object (sdl2-ttf:open-font filename size)))
    (make-instance 'rendered-font :internal-font-object font-object :font-name filename :font-size size)))

(defun free-font (font)
  "Frees all resources used by `FONT'. It can no longer be used."
  (uncache-font font)
  (%free-font font))


;;; Printers

(defmethod print-object ((font rendered-font) stream)
  (print-unreadable-object (font stream :type t :identity t)
    (format stream "~A; ~Apt" (font-name font) (font-size font))))

