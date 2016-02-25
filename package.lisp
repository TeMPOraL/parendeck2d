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
