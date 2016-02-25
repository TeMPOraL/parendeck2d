(in-package #:parendeck2d)

(defparameter *main-window* nil)
(defparameter *gl-context* nil)

;;; configurables
(defparameter *window-width* 800)
(defparameter *window-height* 600)
(defparameter *window-title* "TEST WINDOW")

(defun init-main-window ()
  (log:info "Opening new ~Ax~A window \"~A\"." *window-width* *window-height* *window-title*)
  (setf *main-window* (sdl2:create-window :title *window-title*                                          
                                          :w *window-width*
                                          :h *window-height*
                                          :flags '(:opengl)))
  
  (log:info "Acquiing GL context.")
  (setf *gl-context* (sdl2:gl-create-context *main-window*)))

(defun deinit-main-window ()
  (log:info "Deleting GL context.")
  (sdl2:gl-delete-context *gl-context*)
  
  (log:info "Destroying main window.")
  (sdl2:destroy-window *main-window*))
