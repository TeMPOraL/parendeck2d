(in-package #:parendeck2d)

(defparameter *main-window* nil)
(defparameter *gl-context* nil)

;;; configurables
;;; TODO maybe don't export configurables but instead export getters and setters (that can modify params at runtime if required)
(defparameter *window-width* 800)
(defparameter *window-height* 600)
(defparameter *window-title* "Parendeck 2D")
(defparameter *window-resizable* nil "Whether or not window is created as resizable.")
;;; TODO vsync param

(defparameter *canvas-width* 800 "Width of the virtual canvas - i.e. GL 'screen space' in ortho mode.")
(defparameter *canvas-height* 600 "Height of the virtual canvas - i.e. GL 'screen space' in ortho mode.")

(defun init-main-window ()
  (log:info "Opening new ~Ax~A window \"~A\"." *window-width* *window-height* *window-title*)
  (setf *main-window* (sdl2:create-window :title *window-title*                                          
                                          :w *window-width*
                                          :h *window-height*
                                          :flags (if *window-resizable*
                                                     '(:opengl :resizable)
                                                     '(:opengl))))
  
  (log:info "Acquiring GL context.")
  (setf *gl-context* (sdl2:gl-create-context *main-window*))
  (sdl2:gl-make-current *main-window* *gl-context*)
  (log-opengl-info)
  (init-game-canvas))

(defun deinit-main-window ()
  (log:info "Deleting GL context.")
  (sdl2:gl-delete-context *gl-context*)
  
  (log:info "Destroying main window.")
  (sdl2:destroy-window *main-window*))

(defun log-opengl-info ()
  (log:info "GL version: ~A." (gl:get* :version))
  (log:info "GL vendor: ~A." (gl:get* :vendor))
  (log:info "GL renderer: ~A." (gl:get* :renderer))
  (log:info "GLSL version: ~A." (gl:get* :shading-language-version))
  (log:info "GL extensions: ~A." (gl:get* :extensions))
  (log:info "Swap interval: ~A." (sdl2:gl-get-swap-interval)))

(defun init-game-canvas ()
  (gl:viewport 0 0 *window-width* *window-height*)

  (gl:matrix-mode :projection)
  (gl:ortho 0 *canvas-width* 0 *canvas-height* -2 2) ; (0 0) in lower-left corner, just like on maths lessons
  
  (gl:matrix-mode :modelview)
  (gl:load-identity)

  (gl:clear-color 1 0 1 1)
  (gl:clear :color-buffer))


;;; Utils
;;; FIXME move elsewhere?

(defun window->canvas (x y)
  "Translate coordinates from window space to canvas space."
  (values (float (* p2d:*canvas-width* (/ x p2d:*window-width*)))
          (float (* p2d:*canvas-height* (/ y p2d:*window-height*)))))

(defun canvas->window (x y)
  "Translate coordinates from canvas space to window space."
  (values (float (* p2d:*window-width* (/ x p2d:*canvas-width*)))
          (float (* p2d:*window-height* (/ y p2d:*canvas-height*)))))
