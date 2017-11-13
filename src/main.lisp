(in-package #:parendeck2d)

(defparameter +engine-hello-message+ (concatenate 'string "Parendeck 2D Engine, version " *version*))
(defparameter *game* nil "Game to be run.")

(defparameter *use-fixed-timestep* t)
(defparameter *update-step* (float (/ 1 60)))

(defparameter +max-accumulated-timestep+ 2.0)

(defvar *fps-counter* nil "Counter used to determine running FPS value.")

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



(defun run (&key game profiling-mode)
  "Start the engine. Will load the `GAME' if provided.
If `PROFILING-MODE' is :CPU or :ALLOC, run the game loop under a statistical profiler.
The `PROFILING-MODE' parameter does NOT affect engine's internal profiling and debugging tools."
  (configure-logger)
  
  (log-engine-startup-message)

  (setf *game* (or game
                   (progn
                     (log:warn "No game registered; will use engine default scene.")
                     (setf *game* (make-instance 'default-game)))))

  (preinit *game*)

  (with-engine-initialized
    (init-main-window)
    (initialize *game*)
    (if profiling-mode
        (run-profiled-main-loop profiling-mode)
        (run-main-loop))
    (deinitialize *game*)))

(defun init-engine ()
  "Initialize all engine components."
  (log-sysinfo)

  (p2dprof:clear-all-counters)
  (p2dprof:install-gc-tracker)
  (install-default-asset-search-paths)
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

(defun run-profiled-main-loop (&optional (mode :cpu))
  (p2dprof:with-statistical-profiling (:mode mode)
    (run-main-loop)))

(defun run-main-loop ()
  "Main loop of the engine."

  (log:info "Entering main loop.")

  (let ((dt 0)
        (dt-accumulator 0)
        (last-ticks (get-current-milliseconds))
        (current-ticks (get-current-milliseconds))
        (frames-counter (p2dprof:get-counter 'p2d-frames :interval 1 :description "full frames per second" :history-size 10)))
    (setf *fps-counter* frames-counter)
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

      (:fingermotion
       (:touch-id touch-id :finger-id finger-id :x x :y y :dx dx :dy dy :pressure pressure)
       (on-touch-event *game* touch-id finger-id :move x y dx dy pressure))

      (:fingerdown
       (:touch-id touch-id :finger-id finger-id :x x :y y :dx dx :dy dy :pressure pressure)
       (on-touch-event *game* touch-id finger-id :down x y dx dy pressure))

      (:fingerup
       (:touch-id touch-id :finger-id finger-id :x x :y y :dx dx :dy dy :pressure pressure)
       (on-touch-event *game* touch-id finger-id :up x y dx dy pressure))


      (:idle ()
             (setf current-ticks (get-current-milliseconds)
                   dt (max 0 (msec-delta-in-seconds last-ticks current-ticks))
                   last-ticks current-ticks)

             (when *use-fixed-timestep*
               ;; fixed-step game loop
               (setf dt-accumulator (clamp (+ dt-accumulator dt) 0 +max-accumulated-timestep+))

               (loop while (> dt-accumulator *update-step*) do
                    (p2dprof:with-profiling ('p2d-game-tick :description "main loop single tick (msec/frame)" :history-size 128)
                      (on-tick *game* *update-step*))

                    (p2dprof:sample-appropriate-counters (get-current-seconds) :tick)
                    (decf dt-accumulator *update-step*)))

             (on-idle *game* dt)
             (p2dprof:with-profiling ('p2d-on-render :description "main loop on-render (msec/frame)" :interval :frame :history-size 64)
               (on-render *game* dt))
             (p2dprof:increment-counter frames-counter)

             (p2dprof:sample-appropriate-counters (get-current-seconds) :frame))
      
      (:quit ()
             (on-quit *game*))))
  
  (log:info "Leaving main loop."))

(defun deinit-engine ()
  "Deinitialize the engine."
  (log:info "Deinitializing the engine.")

  (p2dprof:uninstall-gc-tracker)
  (p2dprof:write-counter-report "perf-report.html") ;TODO need a debug/config flag for that at some point, to not dump that on unsuspecting users

  
  (deinit-main-window)
  (p2da:deinitialize-audio)
  (sdl2-ttf:quit)
  (sdl2-image:quit)
  (sdl2:quit)

  (tg:gc :full t)
  (log-tracked-resources-report)
  (log:info "Goodbye!"))


;;; FPS
(defun current-fps ()
  "Return last recorded frames per second value."
  (if *fps-counter*
      (p2dprof:counter-last-sample *fps-counter*)
      0.0))

(defun average-fps ()
  "Returns the running averge of last few frames per second."
  (if *fps-counter*
      (p2dprof:counter-samples-running-avg *fps-counter*)
      0.0))

(defun fps-counter ()
  "Returns the performance counter used to count frames per second."
  *fps-counter*)


;;; other

(defun log-engine-startup-message ()
  (log:info "~A" +engine-hello-message+))

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

