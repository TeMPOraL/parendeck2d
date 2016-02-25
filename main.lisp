(in-package #:parendeck2d)

(defparameter *engine-hello-message* (concatenate 'string "Parenscript 2D Engine, version " *version*))
(defparameter *game* nil "Game to be run.")

(defun register-game (game)
  "Sets the game that will be launched by the engine."
  (setf *game* game))

;;; lifecycle management
(defmacro with-engine-init (&body body)
  `(progn
     (init-engine)
     (unwind-protect
          (progn ,@body)
       (deinit-engine))))

(defun run ()
  "Current entry point into the engine."
  (format t "Hello World~%")

  (unless *game*
    (log:warn "No game registered; will use engine default scene.")
    (setf *game* (make-instance 'default-game)))

  (preinit *game*)

  (with-engine-init
    (initialize *game*)
    (run-main-loop)
    (deinitialize *game*)))

(defun init-engine ()
  "Initialize all engine components."
  (configure-logger)
  (log-engine-startup-message)
  
  (sdl2:init :everything)
  (init-main-window))

(defun run-main-loop ()
  "Main loop of the engine."
  (log:info "Entering main loop.")

  (sdl2:with-event-loop ()

    (:idle ()
           (on-idle *game*))

    (:quit ()
           (on-quit *game*)))
  
  (log:info "Leaving main loop."))

(defun deinit-engine ()
  "Deinitialize the engine."
  (log:info "Deinitializing the engine.")
  (deinit-main-window)
  (sdl2:quit)

  (log:info "Goodbye!"))

;;; other
(defun log-engine-startup-message ()
  (log:info "~A" *engine-hello-message*)

  ;; sysinfo
  (log:info "Running on ~A - ~A ~A ~A ~A ~A."
            (uiop/os:hostname)
            (uiop/os:operating-system)
            (uiop/os:architecture)
            (uiop/os:implementation-type)
            (uiop/os:lisp-version-string)
            (uiop/os:implementation-identifier))

  ;; graphics platform info
  (log:info "Using SDL version ~A.~A.~A" sdl2-ffi:+sdl-major-version+ sdl2-ffi:+sdl-minor-version+ sdl2-ffi:+sdl-patchlevel+)
  (log:debug (sdl2:cpu-count))
  (log:debug (sdl2:cpu-cache-line-size))
  (log:debug (sdl2:alti-vec-p))
  (log:debug (sdl2:mmx-p))
  (log:debug (sdl2:rdtsc-p))
  (log:debug (sdl2:sse-p))
  (log:debug (sdl2:sse2-p))
  (log:debug (sdl2:sse3-p))
  (log:debug (sdl2:sse41-p))
  (log:debug (sdl2:sse42-p)))

