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

(defgeneric on-mouse-event (game mouse-event)
  (:documentation "Lets the `GAME' react to `MOUSE-EVENT'."))

(defgeneric on-key-event (game key-event)
  (:documentation "Lets the `GAME' react to `KEY-EVENT'."))

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

(defmethod on-mouse-event ((game game) mouse-event)
  (declare (ignore game mouse-event))
  (log:debug "The game did not specify mouse event handler"))

(defmethod on-key-event ((game game) key-event)
  (declare (ignore game key-event))
  (log:debug "The game did not specify keyboard event handler"))

(defmethod on-idle ((game game))
  (declare (ignore game))
  (log:trace "The game did not specify idle handler"))

(defmethod on-tick ((game game) dt)
  (declare (ignore game dt))
  (log:trace "The game did not specify tick handler"))

(defmethod on-quit ((game game))
  (declare (ignore game))
  (log:debug "The game did not specify quit handler")
  t)

(defmethod on-render ((game game))
  (declare (ignore game))
  (log:debug "The game did not specify render handler"))


