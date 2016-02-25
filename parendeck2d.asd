;;; parendeck2d.asd

(asdf:defsystem #:parendeck2d
  :serial t
  :long-name "Parendeck 2D game engine"
  :author "Jacek Złydach"
  :version (:read-file-from "version.lisp" :at (1 2 2))
  :description "An engine for 2D games written in Lisp."
                                        ; :long-description "todo"

  :license "MIT"
  :homepage "https://github.com/TeMPOraL/parendeck2d"
  :bug-tracker "https://github.com/TeMPOraL/parendeck2d/issues"
  :source-control (:git "https://github.com/TeMPOraL/parendeck2d.git")
  :mailto "temporal.pl+p2d@gmail.com"

  :encoding :utf-8
  
  :depends-on (#:alexandria
               #:log4cl
               #:sdl2)

  :components ((:file "package")
               (:file "version")
               
               (:module "core"
                        :components ((:file "logger")
                                     (:file "game")))

               (:module "config"
                        :components ((:file "config")
                                     (:file "program-options")))

               (:module "renderer"
                        :components ((:file "renderer")))

               (:module "default-game"
                        :components ((:file "main")))

               (:file "main")))
