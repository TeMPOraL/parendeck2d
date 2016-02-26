(in-package #:parendeck2d)

(defparameter *rotation* 0)

(defclass default-game (game)
  ())

(defmethod preinit ((game default-game))
  (log:info "Default game pre-init."))

(defmethod initialize ((game default-game))
  (log:info "Default game init.")
  
  (setf *rotation* 0))

(defmethod deinitialize ((game default-game))
  (log:info "Default game deinit."))

(defmethod on-mouse-move ((game default-game) x y xrel yrel state))

(defmethod on-mouse-button-event ((game default-game) x y button state)
  (log:info "Default game mouse event.")
  (log:debug x y button state))

(defmethod on-key-event ((game default-game) key state)
  (log:info "Default game key event.")
  (log:debug key state)
  (sdl2:push-event :quit))

(defmethod on-idle ((game default-game)))

(defmethod on-tick ((game default-game) dt)

  (incf *rotation* (* 100 dt))
  (when (> *rotation* 360)
    (decf *rotation* 360)))

(defmethod on-quit ((game default-game))
  (log:info "Default game quit event - quitting.")
  t)

(defmethod on-render ((game default-game))
  (gl:clear :color-buffer)
  (gl:matrix-mode :modelview)
  (gl:load-identity)

  (gl:rotate *rotation* 0 0 1)
  
  (gl:begin :triangles)
  (gl:color 1.0 0.0 0.0)
  (gl:vertex 0.0 1.0)
  (gl:vertex -1.0 -1.0)
  (gl:vertex 1.0 -1.0)
  (gl:end)
  (gl:flush)
  (sdl2:gl-swap-window *main-window*))
