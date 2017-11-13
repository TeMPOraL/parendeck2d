(in-package #:parendeck2d)


;;; Current time getters. Just to standardize on an API.

(declaim (inline get-current-seconds))
(defun get-current-seconds ()
  (declare (optimize (speed 3)))
  "Get a high-resolution time as a float in seconds since some arbitrary point in time.
To be used to compute differences."
  (coerce (/ (sdl2:get-performance-counter)
             (sdl2:get-performance-frequency))
          'double-float))

(declaim (inline get-current-milliseconds))
(defun get-current-milliseconds ()
  (declare (optimize (speed 3)))
  "Gets a high-resolution time as a float in milliseconds since some arbitrary point in time.
To be used to compute differences.

Note that the result is a float - the timer may have higher resolution than 1ms."
  (coerce (* 1000 (/ (sdl2:get-performance-counter)
                     (sdl2:get-performance-frequency)))
          'double-float))


;;; Additional time-related utils will go here.
(declaim (inline msec-delta-in-seconds))
(defun msec-delta-in-seconds (start end)
  "Gets the time passed between `START' msec and `END' msec, converting to seconds."
  (float (/ (- end start) 1000)))
