(in-package #:parendeck2d.graphics)


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
                         :reader internal-font-object)))

(defmethod %free-font ((font rendered-font))
  (sdl2-ttf:close-font (internal-font-object font))
  (setf (slot-value font 'internal-font-object) nil))

(defmethod font-valid-p ((font rendered-font))
  (not (null (internal-font-object font))))

(defmethod render-text ((font rendered-font) text)
  (error "Not yet implemented."))


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
(defvar *font-cache* (make-hash-table :test 'equal) "Hashtable mapping font source filenames to font objects.")

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
(defun get-rendered-font (filename)
  "Get a rendered font from `FILENAME'. Uses a cache to avoid loading and storing the same font data multiple times."
  ;; FIXME add handling/reporting if:
  ;; - an invalid (e.g. freed) font got somehow stuck in cache
  ;; - cached font is not a RENDERED-FONT
  (alexandria:if-let ((font (gethash filename *font-cache*)))
    font
    (setf (gethash filename *font-cache*)
          (make-rendered-font-from-file filename))))

(defun get-bitmap-font (filename)
  "Get a bitmap font from `FILENAME'. Uses a cache to avoid loading and storing the same font data multiple times."
  (error "Not yet implemented."))



;;; Creation / deletion.
(defun make-rendered-font-from-file (filename)
  "Reads the font file `FILENAME' and creates a `RENDERED-FONT' object."
  (let ((font-object (sdl2-ttf:open-font filename 42)))
    (make-instance 'rendered-font :internal-font-object font-object)))

(defun free-font (font)
  "Frees all resources used by `FONT'. It can no longer be used."
  (uncache-font font)
  (%free-font font))


;;; Printers
;;; TODO later.

;;; Some test stuff.
(defun test-render-text-to-texture (rendered-font text)
  (make-texture-from-sdl-surface (sdl2-ttf:render-text-blended (internal-font-object rendered-font) text 255 0 0 255)))
