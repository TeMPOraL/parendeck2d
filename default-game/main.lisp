(in-package #:parendeck2d)

(defparameter *rotation* 0)
(defparameter *dg-ticks-start* 0)
(defparameter *dg-ticks-end* 0)
(defparameter *dg-n-frames* 0)

(defparameter *logo-image* nil)
(defparameter *test-font* nil)
(defparameter *test-rendered-text* nil)

(defparameter *debug-texture-names* '("trc_tex.gif"
                                      "trc_tex.jpg"
                                      "trc_tex.tga"
                                      "trc_tex_24bit.bmp"
                                      "trc_tex_32bit_argb.bmp"
                                      "trc_tex_32bit_xrgb.bmp"
                                      "trc_tex_24bit.png"
                                      "trc_tex_32bit.png"
                                      "trc_tex_lzw.tiff"
                                      "trc_tex_packed.tiff"))

(defparameter *debug-textures* '())

(defun load-debug-images-as-textures ()
  (setf *debug-textures* (mapcar (lambda (file)
                                   (p2dg:get-texture (concatenate 'string "assets/" file)))
                                 *debug-texture-names*)))

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

  (setf *logo-image* (p2dg:get-texture "assets/trc_tex.png"))
  (log:debug *logo-image* (p2dg:width *logo-image*) (p2dg:height *logo-image*) (p2dg:texture-id *logo-image*))

  (setf *test-font* (p2dg::get-rendered-font "assets/FreeSans.ttf")) ;WARNING, FONT NOT COMMITED TO REPO UNTIL LICENSE ISSUES ARE SORTED OUT.
  (log:debug *test-font*)
  (setf *test-rendered-text* (p2dg::test-render-text-to-texture *test-font* "Parendeck 2D"))
  (log:debug *test-rendered-text*)

  (load-debug-images-as-textures))

(defmethod deinitialize ((game default-game))
  (log:info "Default game deinit.")

  (p2dg:free-texture *test-rendered-text*)
  (p2dg::clear-font-cache)
  
  ;; (p2dg:free-texture *logo-image*)
  (p2dg:clear-texture-cache)
  
  (setf *dg-ticks-end* (sdl2:get-ticks))

  (log:info "Got ~A FPS." (float (/ *dg-n-frames* (/ (- *dg-ticks-end* *dg-ticks-start*) 1000)))))

(defmethod on-mouse-move ((game default-game) x y xrel yrel state)
  (declare (ignore game x y xrel yrel state)))

(defmethod on-mouse-wheel-event ((game default-game) x y)
  (log:info "Default game mouse wheel event.")
  (log:debug x y))

(defmethod on-mouse-button-event ((game default-game) x y button state)
  (log:info "Default game mouse button event.")
  (log:debug x y button state))

(defmethod on-key-event ((game default-game) key state repeat)
  (log:info "Default game key event.")
  (log:debug key state repeat)
  (when (= repeat 1) (sdl2:push-event :quit)))

(defmethod on-window-resized ((game default-game) new-width new-height)
  (log:info "Default game window resized event.")
  (log:debug new-width new-height))

(defmethod on-window-mouse-focus ((game default-game) focusedp)
  (log:info "Default game mouse focus event.")
  (log:debug focusedp))

(defmethod on-window-focus ((game default-game) focusedp)
  (log:info "Default game window focus event.")
  (log:debug focusedp))

(defmethod on-window-close ((game default-game))
  (log:info "Default game window close event."))

(defmethod on-idle ((game default-game) dt)
  (declare (ignore game dt)))

(defmethod on-tick ((game default-game) dt)
  (incf *rotation* (* 100 dt))
  (when (> *rotation* 360)
    (decf *rotation* 360)))

(defmethod on-quit ((game default-game))
  (log:info "Default game quit event - quitting.")
  t)

(defmethod on-render ((game default-game) dt)
  (declare (ignore dt))
  (gl:clear :color-buffer)
  (gl:matrix-mode :modelview)
  (gl:load-identity)

  (gl:translate 30 50 0)

  (p2dg:unbind-current-texture)       ;FIXME temporary to disable unwanted texture renders

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

  (gl:translate 100 200 0)

  (gl:with-pushed-matrix
    (gl:scale (float (/ (p2dg:width *test-rendered-text*) 2))
              (float (/ (p2dg:height *test-rendered-text*) 2))
              1)
    (p2dglu:draw-square :texture *test-rendered-text*))

  (gl:translate -200 -300 0)
  (mapc (lambda (tex)
          (gl:with-pushed-matrix
            (gl:scale (float (/ (p2dg:width tex) 4))
                      (float (/ (p2dg:height tex) 4))
                      1)
            (p2dglu:draw-square :texture tex))
          (gl:translate (float (/ (p2dg:width tex) 2)) 0 0))
        *debug-textures*)

  (gl:flush)
  (sdl2:gl-swap-window *main-window*)

  (incf *dg-n-frames*))
