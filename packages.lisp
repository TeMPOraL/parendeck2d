;;; packages.lisp

(defpackage #:parendeck2d
  (:nicknames #:p2d)
  
  (:use #:cl
        #:alexandria)
  
  (:export #:*version*
           #:*main-window*
           #:run

           ;; some configurables (to be later moved elsewhere)
           #:*window-title*
           #:*window-width*
           #:*window-height*
           #:*canvas-width*
           #:*canvas-height*

           #:*use-fixed-timestep*
           #:*max-accumulated-timestep*
           #:*update-step*


           ;; game class
           #:game

           #:preinit
           #:initialize
           #:deinitialize
           #:on-mouse-move
           #:on-mouse-button-event
           #:on-mouse-wheel-event
           #:on-key-event
           #:on-idle
           #:on-tick
           #:on-quit
           #:on-render

           ;; resource
           :resource
           :name
           :loaded
           :loadedp
           :unload-resource
           ))

(defpackage #:parendeck2d.math
  (:nicknames #:p2d.math #:p2dm)

  (:use #:cl)
  (:import-from #:alexandria
                #:define-constant
                #:clamp
                #:rcurry)

  (:export #:+epsilon+
           #:standard-float
           #:+standard-float-zero+
           #:+2pi+
           #:square
           #:clamp
           #:clamp-vector-elements
           #:deg->rad
           #:rad->deg
           #:random-float

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
           #:distance-between-vectors-squared
           #:distance-between-vectors
           #:vector-value-squared
           #:vector-value
           #:normalized-vector
           #:reflect-vector
           #:reflected-vector
           #:rotate-vector-2d
           #:rotated-vector-2d
           #:cross-product
           #:dot-product
           #:⋅
           #:×))

(defpackage #:parendeck2d.graphics
  (:nicknames #:p2d.graphics #:p2d.gfx #:p2dg)
  (:use #:cl
        #:parendeck2d.math)

  (:export
   ;; color
   #:color-4
   #:col-r
   #:col-g
   #:col-b
   #:col-a
   #:make-color-4

   ;; text
   #:+default-text-size+
   #:*default-font*

   #:font
   #:font-name

   #:draw-text
   ))

(defpackage #:parendeck2d.graphics.gl-utils
  (:nicknames #:p2d.graphics.gl-utils #:p2d.gfx.glu #:p2dglu)

  (:use #:cl
        #:parendeck2d.math
        #:parendeck2d.graphics)

  (:export #:translate2
           #:translate3
           #:rotatez*
           #:scale2-uniform
           #:scale3-uniform
           #:color4

           #:draw-circle
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

           #:*ecs-manager*
           #:init-ecs
           #:deinit-ecs
           #:tick
           #:tick-simulation-systems
           #:tick-frame-systems
           #:tick-all-systems))
