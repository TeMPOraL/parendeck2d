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

(defgeneric on-idle (game)
  (:documentation "Called whenever there are no events to process in the engine. The `GAME' can use it to simulate a frame."))

(defgeneric on-tick (game dt)
  (:documentation "Called on specified periods (passed as `DT', in seconds) for configured fixed-step simulation. The `GAME' can use it to simulate a frame."))

(defgeneric on-quit (game)
  (:documentation "Called when the main window receives a quit request. Return NIL to ignore, or T to start shutting down the game and the engine."))

(defgeneric on-render (game)
  (:documentation "Called when it's the best moment for the `GAME' to render its frame."))

;;; default implementations

(defmethod preinit ((game game))
  (declare (ignore game))
  (log:debug "The game did not specify pre-initialization method."))

(defmethod initialize ((game game))
  (declare (ignore game))
  (log:debug "The game did not specify initialization method."))

(defmethod deinitialize ((game game))
  (declare (ignore game))
  (log:debug "The game did not specify deinitialization method."))

(defmethod on-mouse-move ((game game) x y xrel yrel state)
  (declare (ignore game x y xrel yrel state))
  (log:trace "The game did not specify mouse movement event handler."))

(defmethod on-mouse-button-event ((game game) x y button state)
  (declare (ignore game x y button state))
  (log:debug "The game did not specify mouse button event handler."))

(defmethod on-mouse-wheel-event ((game game) x y)
  (declare (ignore game x y))
  (log:debug "The game did not specify mouse wheel event handler."))

(defmethod on-key-event ((game game) key state repeat)
  (declare (ignore game key state repeat))
  (log:debug "The game did not specify keyboard event handler."))

(defmethod on-idle ((game game))
  (declare (ignore game))
  (log:trace "The game did not specify idle handler."))

(defmethod on-tick ((game game) dt)
  (declare (ignore game dt))
  (log:trace "The game did not specify tick handler."))

(defmethod on-quit ((game game))
  (declare (ignore game))
  (log:debug "The game did not specify quit handler.")
  t)

(defmethod on-render ((game game))
  (declare (ignore game))
  (log:debug "The game did not specify render handler"))


