(in-package #:parendeck2d)

(defparameter *engine-hello-message* (concatenate 'string "Parendeck 2D Engine, version " *version*))
(defparameter *game* nil "Game to be run.")

(defparameter *use-fixed-timestep* t)
(defparameter *update-step* (float (/ 1 60)))
(defparameter *max-accumulated-timestep* 2.0)

;;; lifecycle management
(defun call-with-engine-initialized (func)
  (with-logging-conditions (init-engine))
  (unwind-protect
       (sdl2:in-main-thread ()
         (with-logging-conditions
             (funcall func)))
    (sdl2:in-main-thread ()
      (with-logging-conditions
          (deinit-engine)))))

(defmacro with-engine-initialized (&body body)
  `(call-with-engine-initialized (lambda () ,@body)))

(defun run (&optional game)
  "Start the engine. Will load the `GAME' if provided."
  (configure-logger)
  
  (log-engine-startup-message)

  (setf *game* (if game
                   game
                   (progn
                     (log:warn "No game registered; will use engine default scene.")
                     (setf *game* (make-instance 'default-game)))))

  (preinit *game*)

  (with-engine-initialized
    (init-main-window)
    (initialize *game*)
    (run-main-loop)
    (deinitialize *game*)))

(defun init-engine ()
  "Initialize all engine components."
  (log-sysinfo)
  (sdl2:init :everything))

(defun run-main-loop ()
  "Main loop of the engine."
  (log:info "Entering main loop.")


  (let ((dt 0)
        (dt-accumulator 0)
        (last-sdl-ticks 0)
        (current-sdl-ticks 0))
    (sdl2:with-event-loop (:method :poll)

      (:keydown
       (:keysym key :state state :repeat repeat)
       (on-key-event *game* key state repeat))

      (:keyup
       (:keysym key :state state :repeat repeat)
       (on-key-event *game* key state repeat))

      (:mousemotion
       (:x x :y y :xrel xrel :yrel yrel :state state)
       (on-mouse-move *game* x y xrel yrel state))

      (:mousebuttonup
       (:x x :y y :state state :button button)
       (on-mouse-button-event *game* x y button state))
    
      (:mousebuttondown
       (:x x :y y :state state :button button)
       (on-mouse-button-event *game* x y button state))

      (:idle ()
             (when *use-fixed-timestep*
               ;; fixed-step game loop
               (setf current-sdl-ticks (sdl2:get-ticks)
                     dt (max 0 (float (/ (- current-sdl-ticks last-sdl-ticks)
                                         1000)))
                     last-sdl-ticks current-sdl-ticks
                     dt-accumulator (clamp (+ dt-accumulator dt) 0 *max-accumulated-timestep*))

               (loop while (> dt-accumulator *update-step*) do
                    (on-tick *game* *update-step*)
                    (decf dt-accumulator *update-step*)))
             
             (on-idle *game*)
             (on-render *game*))
      (:quit ()
             (on-quit *game*))))
  
  (log:info "Leaving main loop."))

(defun deinit-engine ()
  "Deinitialize the engine."
  (log:info "Deinitializing the engine.")
  (deinit-main-window)
  (sdl2:quit)
  (log:info "Goodbye!"))

;;; other
(defun log-engine-startup-message ()
  (log:info "~A" *engine-hello-message*))

(defun log-sysinfo ()
  (log:info "Running on ~A - ~A ~A ~A ~A ~A."
            (uiop/os:hostname)
            (uiop/os:operating-system)
            (uiop/os:architecture)
            (uiop/os:implementation-type)
            (uiop/os:lisp-version-string)
            (uiop/os:implementation-identifier))

  ;; graphics platform info

  (log:info "Using SDL version ~A.~A.~A" sdl2-ffi:+sdl-major-version+ sdl2-ffi:+sdl-minor-version+ sdl2-ffi:+sdl-patchlevel+)
  (log:info (sdl2:cpu-count))
  (log:info (sdl2:cpu-cache-line-size))
  (log:info (sdl2:alti-vec-p))
  (log:info (sdl2:mmx-p))
  (log:info (sdl2:rdtsc-p))
  (log:info (sdl2:sse-p))
  (log:info (sdl2:sse2-p))
  (log:info (sdl2:sse3-p))
  (log:info (sdl2:sse41-p))
  (log:info (sdl2:sse42-p)))

