(in-package #:parendeck2d)


;;; Current time getters. Just to standardize on an API.

(declaim (inline get-current-milliseconds))
(defun get-current-milliseconds ()
  "Gets a high-resolution time as an integer in milliseconds since some arbitrary point in time.
To be used to compute differences."
  (sdl2:get-ticks))

(declaim (inline get-current-seconds))
(defun get-current-seconds ()
  "Get a high-resolution time as a float in seconds since some arbitrary point in time.
To be used to compute differences."
  (float (/ (get-current-milliseconds)
            1000)))


;;; Maybe, in the future, additional time-related utils will go here.
