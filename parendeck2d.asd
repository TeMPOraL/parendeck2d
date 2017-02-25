;;; parendeck2d.asd

(asdf:defsystem #:parendeck2d
  :serial t
  :long-name "Parendeck 2D game engine"
  :author "Jacek Złydach"
  :version (:read-file-form "version.lisp" :at (1 2 2))
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
               #:sdl2
               #:sdl2-image
               #:sdl2-mixer
               #:sdl2-ttf
               #:cl-opengl
               #:yason
               #:cl-who)

  :components ((:file "packages")
               (:file "version")
               
               (:module "core"
                        :components ((:file "dirs")
                                     (:file "logger")
                                     (:file "printers")
                                     (:file "time")
                                     (:file "game")
                                     (:file "resource-tracker")
                                     (:module "profiler"
                                              :components ((:file "counters")
                                                           (:file "counter-manager")
                                                           (:file "tracing")
                                                           (:file "gc")
                                                           (:file "profiler")))))

               (:module "math"
                        :components ((:file "basic")
                                     (:file "vector")))

               (:module "config"
                        :components ((:file "config")
                                     (:file "program-options")))

               (:module "renderer"
                        :components ((:file "renderer")))

               (:module "graphics"
                        :components ((:file "color")
                                     (:file "texture")
                                     (:module "gl-utils" ;TODO consider deprecating this module altogether
                                              :components ((:file "convenience")
                                                           (:file "shapes")))
                                     (:file "font")
                                     (:file "drawable")
                                     (:file "text")))

               (:module "audio"
                        :components ((:file "system")))

               (:module "ecs"
                        :components ((:file "entity")
                                     (:file "component")
                                     (:file "system")
                                     (:file "manager")))

               (:module "default-game"
                        :components ((:file "main")))

               (:file "main")))
