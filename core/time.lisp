(in-package #:parendeck2d)


;;; Current time getters. Just to standardize on an API.

(declaim (inline get-current-milliseconds))
(defun get-current-milliseconds ()
  "Gets a high-resolution time as an integer in milliseconds since some arbitrary point in time.
To be used to compute differences."
  ;; NOTE we don't worry about sdl2:get-ticks wrapping around, since the documentation says
  ;; it returns an uint32 counting milliseconds from library initialization, which gives us
  ;; around 72 days before wrap-around.
  (sdl2:get-ticks))

(declaim (inline get-current-seconds))
(defun get-current-seconds ()
  "Get a high-resolution time as a float in seconds since some arbitrary point in time.
To be used to compute differences."
  (float (/ (get-current-milliseconds)
            1000)))


;;; Additional time-related utils will go here.
(declaim (inline msec-delta-in-seconds))
(defun msec-delta-in-seconds (start end)
  "Gets the time passed between `START' msec and `END' msec, converting to seconds."
  (float (/ (- end start) 1000)))
