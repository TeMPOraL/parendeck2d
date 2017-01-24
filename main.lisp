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


;;; Temporary - profiling using sb-sprof
#+sbcl
(eval-when (:compile-toplevel :load-toplevel) (require 'sb-sprof))

#+sbcl
(defun run-with-profiling (&optional game)
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
    (sb-sprof:reset)
    (sb-sprof:start-profiling :max-samples 100000)
    (run-main-loop)
    (sb-sprof:stop-profiling)
    (with-open-file (graph-report "sb-sprof.txt" :direction :output :if-exists :supersede)
      (sb-sprof:report :type :graph :stream graph-report))
    (deinitialize *game*)))

#-sbcl
(defun run-with-profiling (&optional game)
  (declare (ignore game))
  (error "Profiling currently not supported on this Lisp implementation."))



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

  (clear-resource-tracking)
  
  (sdl2:init :everything)
  (sdl2-image:init '(:png))
  (sdl2-ttf:init)
  (p2da:initialize-audio))

(defun dispatch-window-event (event window-id data-1 data-2)
  "Get an appropriate callback for window `EVENT'."
  ;; FIXME retyped from SDL library. Make it nicer. Use constants and assocs or macros or sth.
  ;; FIXME should we use window-id for something?
  (cond ((= event 1)                    ;SDL_WINDOWEVENT_SHOWN
         (log:trace "Ignoring WINDOWEVENT_SHOWN." event window-id data-1 data-2))
        ((= event 2)                    ;SDL_WINDOWEVENT_HIDDEN
         (log:trace "Ignoring WINDOWEVENT_HIDDEN." event window-id data-1 data-2))
        ((= event 3)                    ;SDL_WINDOWEVENT_EXPOSED
         (log:trace "Ignoring WINDOWEVENT_EXPOSED." event window-id data-1 data-2))
        ((= event 4)                    ;SDL_WINDOWEVENT_MOVED
         (log:trace "Ignoring WINDOWEVENT_MOVED." event window-id data-1 data-2))
        ((= event 5)                    ;SDL_WINDOWEVENT_RESIZED
         (on-window-resized *game* data-1 data-2))
        ((= event 6)                    ;SDL_WINDOWEVENT_SIZE_CHANGED
         (log:trace "Ignoring WINDOWEVENT_SIZE_CHANGED." event window-id data-1 data-2))
        ((= event 7)                    ;SDL_WINDOWEVENT_MINIMIZED
         (log:trace "Ignoring WINDOWEVENT_MINIMIZED." event window-id data-1 data-2))
        ((= event 8)                    ;SDL_WINDOWEVENT_MAXIMIZED
         (log:trace "Ignoring WINDOWEVENT_MAXIMIZED." event window-id data-1 data-2))
        ((= event 9)                    ;SDL_WINDOWEVENT_RESTORED
         (log:trace "Ignoring WINDOWEVENT_RESTORED." event window-id data-1 data-2))
        ((= event 10)                   ;SDL_WINDOWEVENT_ENTER
         (on-window-mouse-focus *game* t))
        ((= event 11)                   ;SDL_WINDOWEVENT_LEAVE
         (on-window-mouse-focus *game* nil))
        ((= event 12)                   ;SDL_WINDOWEVENT_FOCUS_GAINED
         (on-window-focus *game* t))
        ((= event 13)                   ;SDL_WINDOWEVENT_FOCUS_LOST
         (on-window-focus *game* nil))
        ((= event 14)                   ;SDL_WINDOWEVENT_CLOSE
         (on-window-close *game*))
        (t
         (log:error "Unknown window event." event window-id data-1 data-2))))

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
       (:x x :y y :state state :button button) ;:clicks clicks - for SDL >= 2.0.2
       (on-mouse-button-event *game* x y button state))
    
      (:mousebuttondown
       (:x x :y y :state state :button button) ;:clicks clicks - for SDL >= 2.0.2
       (on-mouse-button-event *game* x y button state))

      (:mousewheel
       (:x x :y y)
       (on-mouse-wheel-event *game* x y))

      (:windowevent
       (:window-id window-id :event event :data1 data-1 :data2 data-2)
       (dispatch-window-event event window-id data-1 data-2))


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

             (on-idle *game* dt)
             (on-render *game* dt))
      (:quit ()
             (on-quit *game*))))
  
  (log:info "Leaving main loop."))

(defun deinit-engine ()
  "Deinitialize the engine."
  (log:info "Deinitializing the engine.")
  (deinit-main-window)
  (p2da:deinitialize-audio)
  (sdl2-ttf:quit)
  (sdl2-image:quit)
  (sdl2:quit)

  (tg:gc :full t)
  (log-tracked-resources-report)
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
  (multiple-value-bind (major minor patch) (sdl2:version)
    (log:info "Using SDL version ~A.~A.~A with wrappings for ~A.~A.~A."
              major minor patch
              sdl2-ffi:+sdl-major-version+ sdl2-ffi:+sdl-minor-version+ sdl2-ffi:+sdl-patchlevel+))
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

