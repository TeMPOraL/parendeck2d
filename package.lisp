;;; package.lisp

(defpackage #:parendeck2d
  (:nicknames :p2d)
  
  (:use #:cl
        #:alexandria)
  
  (:export #:*version*
           #:run
           #:register-game

           #:*main-window*))
