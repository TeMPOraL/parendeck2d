;;;; packages.lisp



(defpackage #:parendeck2d
  (:nicknames #:p2d)
  
  (:use #:cl
        #:alexandria)

  (:export #:ignore-and-log-errors)
  
  (:export #:*version*
           #:*main-window*
           #:run)

  (:export #:track-resource)

  ;; assets
  (:export #:resolve-asset-path)

  ;; some configurables (to be later moved elsewhere)
  (:export #:*window-width*
           #:*window-height*
           #:*window-title*
           #:*window-resizable*
           #:*canvas-width*
           #:*canvas-height*

           #:*use-fixed-timestep*
           #:*max-accumulated-timestep*
           #:*update-step*)


  ;; game class
  (:export #:game

           #:preinit
           #:initialize
           #:deinitialize
           #:on-mouse-move
           #:on-mouse-button-event
           #:on-mouse-wheel-event
           #:on-key-event
           #:on-window-resized
           #:on-window-mouse-focus
           #:on-window-focus
           #:on-window-close
           #:on-idle
           #:on-tick
           #:on-quit
           #:on-render)

  ;; renderer
  (:export #:window->canvas
           #:canvas->window)

  ;; resource
  (:export :resource
           :name
           :loaded
           :loadedp
           :unload-resource)

  ;; time
  (:export #:get-current-milliseconds
           #:get-current-seconds
           #:msec-delta-in-seconds))



(defpackage #:parendeck2d.profiler
  (:nicknames #:p2d.prof #:p2dprof)

  (:use #:cl)

  ;; base counter
  (:export #:counter
           #:increment-counter
           #:counter-samples
           #:counter-samples-running-avg
           #:counter-last-sample
           #:counter-increments
           #:counter-history-size
           #:+default-counter-history-size+)
  
  ;; counter manager
  (:export #:register-counter
           #:get-counter
           #:sample-appropriate-counters
           #:clear-all-counters
           #:write-counter-report)

  ;; GC utilities
  (:export #:install-gc-tracker
           #:uninstall-gc-tracker)

  ;; high-level profiler interface
  (:export #:with-profiling
           #:with-counter
           #:count-value
           #:with-statistical-profiling))



(defpackage #:parendeck2d.math
  (:nicknames #:p2d.math #:p2dm)

  (:use #:cl)
  (:import-from #:alexandria
                #:define-constant
                #:clamp
                #:rcurry)

  ;; generic math utilities
  (:export #:+epsilon+
           #:standard-float
           #:+standard-float-zero+
           #:+pi+
           #:+2pi+
           #:square
           #:clamp                      ; re-exported from Alexandria
           #:clampf
           #:clamp-vector-elements
           #:deg->rad
           #:rad->deg
           #:random-float)

  ;; vectors
  (:export #:vector-2d
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
           #:distance-between-vectors-squared
           #:distance-between-vectors
           #:vector-value-squared
           #:vector-value
           #:normalized-vector
           #:vector-of-length
           #:clamp-vector
           #:clamped-vector
           #:reflect-vector
           #:reflected-vector
           #:rotate-vector-2d
           #:rotated-vector-2d
           #:vector-angle-2d
           #:cross-product
           #:dot-product
           #:⋅
           #:×))



(defpackage #:parendeck2d.graphics
  (:nicknames #:p2d.graphics #:p2d.gfx #:p2dg)
  (:use #:cl
        #:parendeck2d.math)

  ;; color
  (:export #:color-4
           #:col-r
           #:col-g
           #:col-b
           #:col-a
           #:make-color-4
           #:lerp-color)

  ;; texture
  (:export #:texture
           #:width
           #:height
           #:texture-id
           #:clear-texture-cache
           #:bind-texture
           #:unbind-current-texture
           #:texture-valid-p
           #:get-texture
           #:make-texture-from-file
           #:make-texture-from-sdl-surface
           #:make-blank-texture
           #:free-texture
           #:with-texture)

  ;; font
  (:export #:*default-font-size*
           #:font
           #:font-valid-p
           #:render-text
           #:rendered-font
           #:bitmap-font
           #:clear-font-cache
           #:get-rendered-font
           #:get-bitmap-font
           #:make-rendered-font-from-file
           #:free-font)

  ;; drawables
  (:export #:drawable
           #:draw
           #:draw-rectangle
           #:draw-rectangle-outline
           #:with-color)

  ;; text
  (:export #:text
           #:rendered-text))



(defpackage #:parendeck2d.graphics.gl-utils
  (:nicknames #:p2d.graphics.gl-utils #:p2d.gfx.glu #:p2dglu)

  (:use #:cl
        #:parendeck2d.math
        #:parendeck2d.graphics)

  ;; generic GL utilities
  (:export #:translate2
           #:translate3
           #:rotatez*
           #:scale2-uniform
           #:scale3-uniform
           #:color4)

  ;; shapes
  (:export #:draw-circle
           #:draw-circle-outline
           #:draw-triangle
           #:draw-triangle-outline
           #:draw-square
           #:draw-square-outline
           #:draw-regular-polygon
           #:draw-regular-polygon-outline))



(defpackage #:parendeck2d.audio
  (:nicknames #:p2d.audio #:p2d.sfx #:p2da)

  (:use #:cl)
  (:export #:initialize-audio           ;FIXME technically not public interface outside engine
           #:deinitialize-audio))



(defpackage #:parendeck2d.ecs
  (:nicknames #:p2d.ecs #:p2de)

  (:use #:cl
        #:alexandria)

  ;; ECS stuff
  ;; FIXME some of these are implementation details and should be hidden
  (:export #:entity
           #:entity-id
           #:tag
           #:components
           #:component-names
           #:add-component
           #:remove-component
           #:find-component
           #:register-entity
           #:unregister-entity
           #:unregister-entity*
           #:tag-entity
           #:tag-entity*
           #:make-entity
           #:schedule-entity-for-deletion
           #:batch-make-entities
           #:entity-by-id
           #:entity-by-tag

           #:component
           #:defcomponent

           #:system
           #:find-system
           #:defsystem
           #:register-system
           #:do-system
           #:entity-added
           #:entity-removed)

  ;; ECS manager
  (:export #:*ecs-manager*
           #:init-ecs
           #:deinit-ecs
           #:tick
           #:tick-simulation-systems
           #:tick-frame-systems
           #:tick-all-systems
           #:schedule-all-entities-for-deletion))
