(in-package #:parendeck2d)

(defclass default-game (game)
  ())

(defmethod preinit ((game default-game))
  (log:info "Default game pre-init."))

(defmethod initialize ((game default-game))
  (log:info "Default game init."))

(defmethod deinitialize ((game default-game))
  (log:info "Default game deinit."))

(defmethod on-mouse-event ((game default-game) mouse-event)
  (log:info "Default game mouse event."))

(defmethod on-key-event ((game default-game) key-event)
  (log:info "Default game key event."))

(defmethod on-idle ((game default-game)))

(defmethod on-tick ((game default-game) dt))

(defmethod on-quit ((game default-game))
  (log:info "Default game quit event - quitting.")
  t)

(defmethod on-render ((game default-game)))

