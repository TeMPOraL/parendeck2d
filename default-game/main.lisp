(in-package #:parendeck2d)

(defparameter *rotation* 0)
(defparameter *dg-ticks-start* 0)
(defparameter *dg-ticks-end* 0)
(defparameter *dg-n-frames* 0)

(defparameter *logo-image* nil)

(defclass default-game (game)
  ())

(defmethod preinit ((game default-game))
  (log:info "Default game pre-init.")
  (setf *window-title* "Parendeck 2D - no game loaded"))

(defmethod initialize ((game default-game))
  (log:info "Default game init.")

  (gl:clear-color 0.0 0.0 0.0 1.0)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:enable :blend)
  (gl:enable :line-smooth)
  (gl:hint :line-smooth-hint :nicest)
  (gl:enable :polygon-smooth)
  (gl:hint :polygon-smooth-hint :nicest)

  (gl:enable :texture-2d)
  
  (setf *rotation* 0)
  (setf *dg-ticks-start* (sdl2:get-ticks))

  (setf *logo-image* (p2dg::load-image-from-file "trc_tex.png"))

  (log:debug *logo-image* (p2dg::width *logo-image*) (p2dg::height *logo-image*) (p2dg::texture-id *logo-image*)))

(defmethod deinitialize ((game default-game))
  (log:info "Default game deinit.")

  (unload-resource *logo-image*)
  
  (setf *dg-ticks-end* (sdl2:get-ticks))

  (log:info "Got ~A FPS." (float (/ *dg-n-frames* (/ (- *dg-ticks-end* *dg-ticks-start*) 1000)))))

(defmethod on-mouse-move ((game default-game) x y xrel yrel state))

(defmethod on-mouse-button-event ((game default-game) x y button state)
  (log:info "Default game mouse event.")
  (log:debug x y button state))

(defmethod on-key-event ((game default-game) key state repeat)
  (log:info "Default game key event.")
  (log:debug key state repeat)
  (when (= repeat 1) (sdl2:push-event :quit)))

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

  (gl:translate 30 50 0)

  (gl:bind-texture :texture-2d 0)       ;FIXME temporary to disable unwanted texture renders

  (gl:with-pushed-matrix
    (gl:rotate *rotation* 0 0 1)
    (gl:scale 10 10 10 )
    (p2dglu:draw-circle))

  (gl:translate 30 0 0)

  (gl:with-pushed-matrix
    (gl:rotate *rotation* 0 0 1)
    (gl:scale 10 10 10)
    (p2dglu:draw-circle-outline))

  (gl:translate 30 0 0)

  (gl:with-pushed-matrix
    (gl:rotate *rotation* 0 0 1)
    (gl:scale 10 10 10)
    (p2dglu:draw-triangle))

  (gl:translate 30 0 0)

  (gl:with-pushed-matrix
    (gl:rotate *rotation* 0 0 1)
    (gl:scale 10 10 10)
    (p2dglu:draw-triangle-outline))

  (gl:translate 30 0 0)

  (gl:with-pushed-matrix
    (gl:rotate *rotation* 0 0 1)
    (gl:scale 10 10 10)
    (p2dglu:draw-square))

  (gl:translate 30 0 0)

  (gl:with-pushed-matrix
    (gl:rotate *rotation* 0 0 1)
    (gl:scale 10 10 10)
    (p2dglu:draw-square-outline))

  (gl:translate 30 0 0)

  (gl:with-pushed-matrix
    (gl:rotate *rotation* 0 0 1)
    (gl:scale 10 10 10)
    (p2dglu:draw-regular-polygon 5))

  (gl:translate 30 0 0)

  (gl:with-pushed-matrix
    (gl:rotate *rotation* 0 0 1)
    (gl:scale 10 10 10)
    (p2dglu:draw-regular-polygon-outline 6))

  (gl:translate 0 200 0)

  (gl:with-pushed-matrix
    (gl:rotate *rotation* 0 0 1)
    (gl:scale 100 100 100)
    (p2dglu:draw-square :texture *logo-image*))

  (gl:flush)
  (sdl2:gl-swap-window *main-window*)

  (incf *dg-n-frames*))
