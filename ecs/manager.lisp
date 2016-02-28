(in-package :parendeck2d.ecs)

(defvar *ecs-manager* nil)

(defclass ecs-manager ()
  ((entity-count :accessor entity-count ;TODO rename + add support for entity reuse
                 :initform 0)
   (entities :accessor entities
             :initform (make-hash-table))
   (tags :accessor tags
         :initform (make-hash-table))
   (systems :accessor systems
            :initform nil)))

(defun init-ecs ()
  (log:info "Initializing ECS subsystem.")
  (setf *ecs-manager* (make-instance 'ecs-manager)))

(defun deinit-ecs ()
  (log:info "Deinitializing ECS subsystem.")
  (setf *ecs-manager* nil))

(defun get-new-entity-id ()
  "Allocates a new ID for an entity; reuses a freed ID if available, otherwise generates a new one."
  (error "Not yet implemented."))

;;; FIXME this *needs* to be done differently
(defun tick (dt)
  "Used to step through all systems each iteration of the game loop."
  (dolist (s (systems *ecs-manager*))
    (dolist (id (entities s))
      (do-system s (entity-by-id id dt)))))

;;; different attempt
(defun tick-simulation-systems (dt)
  "Ticks all the systems that are supposed to run in simulation time (e.g. logic, physics, etc.)."
  ;; TODO
  (declare (ignore dt)))

(defun tick-frame-systems (dt)
  "Ticks all the systems that are supposed to run in render time."
  ;; TODO
  (declare (ignore dt)))

(defun tick-all-systems (dt)
  "Ticks all systems."
  (declare (ignore dt)))
