(in-package #:parendeck2d)

(defun run ()
  "Current entry point into the engine."
  (format t "Hello World~%")

  ;; Let's define the engine lifecycle.
  (pre-init-game)
  (init-engine)
  (init-game)

  (run-main-loop)

  (deinit-game)
  (deinit-engine))

(defun pre-init-game ()
  "Do an early preinitialization of the game in case it wants to configure the engine before it starts."
  nil)

(defun init-engine ()
  "Initialize all engine components."
  nil)

(defun init-game ()
  "Initialize the game."
  nil)

(defun run-main-loop ()
  "Main loop of the engine."
  nil)

(defun deinit-game ()
  "Deinitialize the game."
  nil)

(defun deinit-engine ()
  "Deinitialize the engine."
  nil)
