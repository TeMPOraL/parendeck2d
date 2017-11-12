(in-package #:parendeck2d)

(defparameter *rotation* 0)
(defparameter *dg-ticks-start* 0)
(defparameter *dg-ticks-end* 0)
(defparameter *dg-n-frames* 0)

(defparameter *logo-image* nil)
(defparameter *test-font* nil)
(defparameter *test-rendered-text* nil)
(defparameter *test-rtext* nil)

(defparameter *accumulator* 0.0)

(defparameter *touch-marker-position* (p2dm:make-vector-2d 100.0 100.0))
(defparameter *last-touch-pressure* 0.0)

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

  (gl:enable :texture-2d)
  
  (setf *rotation* 0)
  (setf *dg-ticks-start* (get-current-milliseconds))

  (setf *logo-image* (p2dg:get-texture "assets/trc_tex_transparent.png"))
  (log:debug *logo-image* (p2dg:width *logo-image*) (p2dg:height *logo-image*) (p2dg:texture-id *logo-image*))

  (setf *test-font* (p2dg:get-rendered-font "assets/fonts/Vera.ttf" :size 32))
  (log:debug *test-font*)

  (load-debug-images-as-textures)

  (setf *test-rendered-text* (p2dg:render-text *test-font* "Parendeck 2D"))
  (setf *test-rtext* (p2dg:render-text *test-font* "hello!"))

  (log:info (gl:get-float :current-color)))

(defmethod deinitialize ((game default-game))
  (log:info "Default game deinit.")

  (setf *test-rendered-text* nil)
  (setf *test-rtext* nil)

  (p2dg:clear-font-cache)
  
  ;; (p2dg:free-texture *logo-image*)
  (p2dg:clear-texture-cache)
  
  (setf *dg-ticks-end* (get-current-milliseconds))

  (log:info "Got ~A FPS." (/ *dg-n-frames* (msec-delta-in-seconds *dg-ticks-start* *dg-ticks-end*))))

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

(defmethod on-touch-event ((game default-game) touch-id finger-id direction x y dx dy pressure)
  (log:info "Default game touch event.")
  (when (eq direction :down)
    (setf *touch-marker-position* (p2dm:make-vector-2d (* x *canvas-width*)
                                                       (* y *canvas-height*))
          *last-touch-pressure* pressure)
    (log:info *touch-marker-position* *last-touch-pressure*)))

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
  (declare (ignorable dt))

  (p2dg:unbind-current-texture)

  (p2dg:with-color (1.0 1.0 1.0)
    (gl:with-pushed-matrix
      (gl:translate (p2dm:vec-x *touch-marker-position*)
                    (p2dm:vec-y *touch-marker-position*)
                    0.0)
      (gl:scale (+ 20 (* 20 *last-touch-pressure*))
                (+ 20 (* 20 *last-touch-pressure*))
                0)
      (p2dglu:draw-circle)))

  (p2dg:with-color (0.0 1.0 0.0)
    (p2dg:draw *test-rendered-text* :x 100.0 :y 300.0 :scale-y 2.0))

  (incf *accumulator* dt)
  (p2dg:with-color (1.0 1.0 0.0)
    (p2dg::draw-text (format nil "FPS (avg): ~A" (average-fps)) :font *test-font* :x 200 :y 400)
    (p2dg::draw-text (format nil "FPS (cur): ~A" (current-fps)) :font *test-font* :x 150 :y 450 :rotation *rotation*))

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

  (p2dg:with-color (1.0 0.0 0.0)
    (p2dg:draw *test-rtext* :x -100.0 :y -400.0 :rotation *rotation*))


  (gl:translate -200 -300 0)
  (mapc (lambda (tex)
          (gl:with-pushed-matrix
            (gl:scale (float (/ (p2dg:width tex) 4))
                      (float (/ (p2dg:height tex) 4))
                      1)
            (p2dglu:draw-square :texture tex))
          (gl:translate (float (/ (p2dg:width tex) 2)) 0 0))
        *debug-textures*)

  (incf *dg-n-frames*))
