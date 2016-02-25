(in-package #:parendeck2d)

(defparameter *main-window* nil)
(defparameter *gl-context* nil)

;;; configurables
(defparameter *window-width* 800)
(defparameter *window-height* 600)
(defparameter *window-title* "TEST WINDOW")

(defparameter *canvas-width* 800 "Width of the virtual canvas - i.e. GL 'screen space' in ortho mode.")
(defparameter *canvas-height* 600 "Height of the virtual canvas - i.e. GL 'screen space' in ortho mode.")

(defun init-main-window ()
  (log:info "Opening new ~Ax~A window \"~A\"." *window-width* *window-height* *window-title*)
  (setf *main-window* (sdl2:create-window :title *window-title*                                          
                                          :w *window-width*
                                          :h *window-height*
                                          :flags '(:opengl)))
  
  (log:info "Acquiing GL context.")
  (setf *gl-context* (sdl2:gl-create-context *main-window*))
  (sdl2:gl-make-current *main-window* *gl-context*)
  (init-opengl))

(defun deinit-main-window ()
  (log:info "Deleting GL context.")
  (sdl2:gl-delete-context *gl-context*)
  
  (log:info "Destroying main window.")
  (sdl2:destroy-window *main-window*))

(defun init-opengl ()
  (gl:viewport 0 0 *window-width* *window-height*)

  (gl:matrix-mode :projection)
  (gl:ortho 0 *canvas-width* *canvas-height* 0 -1 1)
  
  (gl:matrix-mode :modelview)
  (gl:load-identity)

  (gl:clear-color 1 0 1 1)
  (gl:clear :color-buffer))
