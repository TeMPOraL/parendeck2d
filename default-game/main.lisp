(in-package #:parendeck2d)

(defclass default-game (game)
  ())

(defmethod preinit ((game default-game))
  (log:info "Default game pre-init."))

(defmethod initialize ((game default-game))
  (log:info "Default game init."))

(defmethod deinitialize ((game default-game))
  (log:info "Default game deinit."))

(defmethod on-mouse-move ((game default-game) x y xrel yrel state))

(defmethod on-mouse-button-event ((game default-game) x y button state)
  (log:info "Default game mouse event.")
  (log:debug x y button state))

(defmethod on-key-event ((game default-game) key state)
  (log:info "Default game key event.")
  (log:debug key state))

(defmethod on-idle ((game default-game)))

(defmethod on-tick ((game default-game) dt))

(defmethod on-quit ((game default-game))
  (log:info "Default game quit event - quitting.")
  t)

(defmethod on-render ((game default-game)))

