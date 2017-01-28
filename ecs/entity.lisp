(in-package #:parendeck2d.ecs)

(defclass entity ()
  ((id :reader entity-id
       :initform (incf (entity-count *ecs-manager*))) ;TODO make manager recycle entity ids
   (tag :accessor tag
        :initform nil)
   (components :accessor components
               :initform nil)))

(defun component-names (entity)
  "Return a list of names of the components for an `ENTITY'."
  (mapcar #'name (components entity)))

(defun update-system (entity)
  "Register or de-register an `ENTITY' with all applicable systems.
   Called when a component is added or removed from an entity."
  (dolist (s (systems *ecs-manager*))
    (if (subsetp (required s) (component-names entity))
        (unless (member (entity-id entity) (entities s)) ;FIXME checking twice (1/2)
          (entity-added s entity)
          (pushnew (entity-id entity) (entities s)))
        (when (member (entity-id entity) (entities s)) ;FIXME checking twice (2/2)
          (entity-removed s entity)
          (deletef (entities s) (entity-id entity))))))

(defun add-component (entity name &rest args)
  "Add a component by `NAME' to an `ENTITY' and update all systems."
  (when-let ((component (make-component name args)))
    (pushnew component (components entity) :key #'name)
    (update-system entity)))

(defun remove-component (entity name)
  "Remove a component by `NAME' from an `ENTITY' and update all systems."
  (deletef (components entity) name :key #'name)
  (update-system entity)
  (unless (components entity)
    (unregister-entity* (entity-id entity))))

(defun register-entity (entity)         ;TODO move to manager
  "Register an `ENTITY' with the manager."
  (setf (gethash (entity-id entity) (entities *ecs-manager*)) entity))

(defun unregister-entity (entity)           ;TODO move to manager
  "Unregister an `ENTITY' with the manager."
  (unregister-entity* (entity-id entity)))

(defun unregister-entity* (id)           ;TODO move to manager
  "Unregister an entity with a given `ID' with the manager."
  (remhash id (entities *ecs-manager*)))

(defun tag-entity* (id tag)              ;TODO maybe split to this and manager part
  "Tag an entity."
  (let ((entity (entity-by-id id)))
    (when (tag entity)
      (untag-entity* id tag))
    (setf (gethash tag (tags *ecs-manager*)) entity)))

(defun untag-entity* (id tag)            ;TODO maybe split to this and manager part
  "Untag an entity."
  (let ((entity (entity-by-id id)))
    (setf (tag entity) nil)
    (remhash tag (tags *ecs-manager*))))

(defun find-component (entity name)
  "Find a component of an `ENTITY' given its `NAME'."
  (find name (components entity) :key #'name))

(defun make-entity (&key tag components)
  "Create a new entity, then optionally `TAG' it and add `COMPONENTS'."
  (let ((entity (make-instance 'entity)))
    (register-entity entity)
    (dolist (name components)
      (add-component entity name))
    (when tag
      (tag-entity* (entity-id entity) tag))
    entity))

(defun delete-entity (entity)
  "Deletes the entity from the ECS manager and all systems. Called
automatically by the ECS manager. To delete an entity from within the `DO-SYSTEM' code,
use `SCHEDULE-ENTITY-FOR-DELETION' instead."
  (dolist (component (copy-list (components entity)))
    (remove-component entity (name component)))
  (when (tag entity)
    (untag-entity* (entity-id entity) (tag entity))))

(defun schedule-entity-for-deletion (entity)
  (pushnew entity (to-delete *ecs-manager*) :key #'entity-id))

(defmacro batch-make-entities ((&rest items) (&rest components) &body body)
  "Macro for creating multiple entities with the same components."
  `(let (,@(loop for i in items
                 for e = `(make-entity :components ',components)
                 collect (list i e)))
     ,@body))

(defun entity-by-id (id)
  "Find an entity by its ID."
  (gethash id (entities *ecs-manager*)))

(defun entity-by-tag (tag)
  "Find an entity by its tag."
  (gethash tag (tags *ecs-manager*)))

(defmethod print-object ((object entity) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "#~A [~A] (~{~A~^ ~})" (entity-id object) (tag object) (component-names object))))
