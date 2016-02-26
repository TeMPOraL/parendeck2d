;;; package.lisp

(defpackage #:parendeck2d
  (:nicknames :p2d)
  
  (:use #:cl
        #:alexandria)
  
  (:export #:*version*
           #:*main-window*
           #:run

           ;; some configurables (to be later moved elsewhere)
           #:*window-title*
           #:*use-fixed-timestep*
           #:*max-accumulated-timestep*
           #:*update-step*

           ;; math
           #:+epsilon+
           #:standard-float
           #:+standard-float-zero+
           #:square
           #:clamp-vector-elements
           #:deg->rad
           #:rad->deg

           #:vector-2d
           #:vector-3d
           #:vector-4d
           #:vec-x
           #:vec-y
           #:vec-z
           #:vec-w
           #:make-vector-2d
           #:make-vector-3d
           #:make-vector-4d
           #:vector-2d-p
           #:vector-3d-p
           #:vector-4d-p
           #:add-vectors
           #:add-to-vector
           #:subtract-vectors
           #:subtract-from-vector
           #:scaled-vector
           #:scale-vector
           #:negative-vector
           #:negate-vector
           #:distance-between-vectors
           #:vector-value-squared
           #:vector-value
           #:normalized-vector
           #:reflect-vector
           #:cross-product
           #:dot-product
           #:⋅
           #:×
           

           ;; game class
           #:game

           #:preinit
           #:initialize
           #:deinitialize
           #:on-mouse-event
           #:on-key-event
           #:on-idle
           #:on-tick
           #:on-quit
           #:on-render))
