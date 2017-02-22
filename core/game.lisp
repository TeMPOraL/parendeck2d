(in-package #:parendeck2d)

(defclass game ()
  ())

(defgeneric preinit (game)
  (:documentation "Pre-initialization step for `GAME'. Called before
  full initialization of the engine, and can be used to configure it."))

(defgeneric initialize (game)
  (:documentation "Initialize `GAME'. Called after engine initialization."))

(defgeneric deinitialize (game)
  (:documentation "Deinitialize `GAME'. Called before engine deinitialization."))

(defgeneric on-mouse-move (game x y xrel yrel state)
  (:documentation "Lets the `GAME' react to mouse movements. `X' and
  `Y' are current cursor positions. `XREL' and `YREL' are the relative
  movements the mouse did since the last call to this event. `STATE'
  is the button state."))

(defgeneric on-mouse-button-event (game x y button state)
  (:documentation "Lets the `GAME' react to mouse button presses."))

(defgeneric on-mouse-wheel-event (game x y)
  (:documentation "Lets the `GAME' react to mouse wheel events."))

(defgeneric on-key-event (game key state repeat)
  (:documentation "Lets the `GAME' react to the `STATE' of a `KEY' and know if it was a `REPEAT'ed press."))

(defgeneric on-window-resized (game new-width new-height)
  (:documentation "Called when window was resized to `NEW-WIDTH' x `NEW-HEIGHT'."))

(defgeneric on-window-mouse-focus (game focusedp)
  (:documentation "Called when mouse pointer gets over or out of the window area."))

(defgeneric on-window-focus (game focusedp)
  (:documentation "Called when window receives or loses focus."))

(defgeneric on-window-close (game)
  (:documentation "Called when user requested closing game window."))

(defgeneric on-idle (game dt)
  (:documentation "Called whenever there are no events to process in the engine. The `GAME' can use it to simulate a frame."))

(defgeneric on-tick (game dt)
  (:documentation "Called on specified periods (passed as `DT', in seconds) for configured fixed-step simulation. The `GAME' can use it to simulate a frame."))

(defgeneric on-quit (game)
  (:documentation "Called when the main window receives a quit request. Return NIL to ignore, or T to start shutting down the game and the engine."))

(defgeneric on-render (game dt)
  (:documentation "Called when it's the best moment for the `GAME' to render its frame."))

;;; default implementations

(defmethod preinit ((game game))
  (declare (ignore game))
  (log:trace "The game did not specify pre-initialization method."))

(defmethod initialize ((game game))
  (declare (ignore game))
  (log:trace "The game did not specify initialization method."))

(defmethod deinitialize ((game game))
  (declare (ignore game))
  (log:trace "The game did not specify deinitialization method."))

(defmethod on-mouse-move ((game game) x y xrel yrel state)
  (declare (ignore game x y xrel yrel state))
  (log:trace "The game did not specify mouse movement event handler."))

(defmethod on-mouse-button-event ((game game) x y button state)
  (declare (ignore game x y button state))
  (log:trace "The game did not specify mouse button event handler."))

(defmethod on-mouse-wheel-event ((game game) x y)
  (declare (ignore game x y))
  (log:trace "The game did not specify mouse wheel event handler."))

(defmethod on-key-event ((game game) key state repeat)
  (declare (ignore game key state repeat))
  (log:trace "The game did not specify keyboard event handler."))

(defmethod on-window-resized ((game game) new-width new-height)
  (declare (ignore new-width new-height))
  (log:trace "The game did not specify window resized handler."))

(defmethod on-window-mouse-focus ((game game) focusedp)
  (declare (ignore game focusedp))
  (log:trace "The game did not specify mouse focus handler."))

(defmethod on-window-focus ((game game) focusedp)
  (declare (ignore game focusedp))
  (log:trace "The game did not specify window focus handler."))

(defmethod on-window-close ((game game))
  (declare (ignore game))
  (log:trace "The game did not specify window close handler."))

(defmethod on-idle ((game game) dt)
  (declare (ignore game dt))
  (log:trace "The game did not specify idle handler."))

(defmethod on-tick ((game game) dt)
  (declare (ignore game dt))
  (log:trace "The game did not specify tick handler."))

(defmethod on-quit ((game game))
  (declare (ignore game))
  (log:trace "The game did not specify quit handler.")
  t)

(defmethod on-render ((game game) dt)
  (declare (ignore game dt))
  (log:trace "The game did not specify render handler"))


;;; Code (meant to be) common across all games.

(defmethod on-render :before ((game game) dt)
  (declare (ignore game dt))
  (gl:clear :color-buffer)
  (gl:matrix-mode :modelview)
  (gl:load-identity))

(defmethod on-render :after ((game game) dt)
  (declare (ignore game dt))
  (gl:flush)
  (sdl2:gl-swap-window *main-window*))
