(in-package :parendeck2d.ecs)

(defvar *ecs-manager* nil)

(defclass ecs-manager ()
  ((entity-count :accessor entity-count ;TODO rename + add support for entity reuse
                 :initform 0)
   (entities :accessor entities
             :initform (make-hash-table))
   (to-delete :accessor to-delete
              :initform nil)
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

(defun delete-entities-scheduled-for-deletion ()
  (dolist (entity (to-delete *ecs-manager*))
    (delete-entity entity))
  (setf (to-delete *ecs-manager*) '()))

;;; FIXME this *needs* to be done differently
(defun tick (type dt)
  "Used to step through all systems of given `TYPE'."
  (dolist (s (systems *ecs-manager*))
    (when (eql (system-type s) type)
      (p2dprof:with-profiling ((alexandria:ensure-symbol (alexandria:symbolicate "ECS-TICK-" (class-name (class-of s)))
                                                         :parendeck2d.ecs)
                               :description "ECS system ticks/frame (msec/frame)"
                               :interval :frame)
       (dolist (id (entities s))
         (do-system s (entity-by-id id) dt)))))
  ;; FIXME need one well-defined moment for deleting entities!
  ;; (right now, entities are deleted at the end of ticking of a particular type of system)
  (delete-entities-scheduled-for-deletion))

;;; different attempt
(defun tick-simulation-systems (dt)
  "Ticks all the systems that are supposed to run in simulation time (e.g. logic, physics, etc.)."
  (tick :simulation dt))

(defun tick-frame-systems (dt)
  "Ticks all the systems that are supposed to run in render time."
  (tick :frame dt))

(defun tick-all-systems (dt)
  "Ticks all systems."
  (tick-simulation-systems dt)
  (tick-frame-systems dt))

(defun schedule-all-entities-for-deletion ()
  (maphash (lambda (id entity)
             (log:trace "Scheduling entity #~A for deletion." id)
             (schedule-entity-for-deletion entity))
           (entities *ecs-manager*)))
