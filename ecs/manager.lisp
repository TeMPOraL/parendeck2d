(in-package :parendeck2d.ecs)

(defclass ecs-manager ()
  ((entity-count :accessor entity-count ;TODO rename + add support for entity reuse
                 :initform 0)
   (entities :accessor entities
             :initform (make-hash-table))
   (tags :accessor tags
         :initform (make-hash-table))
   (systems :accessor systems
            :initform nil)))

(defvar *ecs-manager* (make-instance 'ecs-manager))

(defun get-new-entity-id ()
  "Allocates a new ID for an entity; reuses a freed ID if available, otherwise generates a new one."
  )

;;; FIXME this *needs* to be done differently
(defun tick ()
  "Used to step through all systems each iteration of the game loop."
  (dolist (s (systems *ecs-manager*))
    (dolist (id (entities s))
      (do-system s (entity-by-id id)))))
