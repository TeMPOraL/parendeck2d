(in-package #:parendeck2d.graphics.gl-utils)

(defun draw-circle (&key (resolution 64) (textured nil))
  (when textured
    (error "Textured shapes not yet implemented."))
  
  (let ((step-angle (coerce (/ +2pi+ resolution) 'standard-float)))
    (gl:with-primitive :polygon
      (dotimes (step resolution)
        (gl:vertex (cos (* step step-angle)) (sin (* step step-angle)))))))

(defun draw-circle-outline (&key (resolution 64))
  (let ((step-angle (coerce (/ +2pi+ resolution) 'standard-float)))
    (gl:with-primitive :line-loop
      (dotimes (step resolution)
        (gl:vertex (cos (* step step-angle)) (sin (* step step-angle)))))))

(defun draw-triangle (&key (textured nil))
  (when textured
    (error "Textured shapes not yet implemented."))
  
  (gl:with-primitive :triangles
    (gl:vertex 0.0 1.0)
    (gl:vertex -0.8660254 -0.50000006)
    (gl:vertex 0.8660255 -0.49999994)))

(defun draw-triangle-outline ()
  (gl:with-primitive :line-loop
    (gl:vertex 0.0 1.0)
    (gl:vertex -0.8660254 -0.50000006)
    (gl:vertex 0.8660255 -0.49999994)))

(defun draw-square (&key (texture nil))
  ;; (if texture
  ;;     (gl:bind-texture :texture-2d (p2dg::texture-id texture))
  ;;     (gl:bind-texture :texture-2d 0))
  
  (gl:with-primitive :quads
    (gl:tex-coord 1.0 0.0)
    (gl:vertex 1.0 -1.0)
    (gl:tex-coord 1.0 1.0)
    (gl:vertex 1.0 1.0)
    (gl:tex-coord 0.0 1.0)
    (gl:vertex -1.0 1.0)
    (gl:tex-coord 0.0 0.0)
    (gl:vertex -1.0 -1.0)))

(defun draw-square-outline ()
  (gl:with-primitive :line-loop
    (gl:vertex 1.0 -1.0)
    (gl:vertex 1.0 1.0)
    (gl:vertex -1.0 1.0)
    (gl:vertex -1.0 -1.0)))

(defun draw-regular-polygon (sides &key (textured nil))
  (when textured
    (error "Textured shapes not yet implemented."))

  ;; yes, I know it's basically the same function as circle
  (let* ((step-angle (coerce (/ +2pi+ sides) 'standard-float))
         (adjustment (/ step-angle 2)))
    (gl:with-pushed-matrix
      (gl:rotate adjustment 0 0 1)
      (gl:with-primitive :polygon
        (dotimes (step sides)
          (gl:vertex (cos (* step step-angle)) (sin (* step step-angle))))))))

(defun draw-regular-polygon-outline (sides)
  ;; yes, I know it's basically the same function as circle-outline
  (let* ((step-angle (coerce (/ +2pi+ sides) 'standard-float))
         (adjustment (/ step-angle 2)))
    (gl:with-pushed-matrix
      (gl:rotate adjustment 0 0 1)
      (gl:with-primitive :line-loop
        (dotimes (step sides)
          (gl:vertex (cos (* step step-angle)) (sin (* step step-angle))))))))
