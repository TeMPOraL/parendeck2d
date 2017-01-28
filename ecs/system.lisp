(in-package #:parendeck2d.ecs)

(defparameter +default-system-priority+ 0)

(defclass system ()
  ((name :reader name)
   (required :accessor required
             :initform nil)
   (entities :accessor entities
             :initform nil)
   (system-type :accessor system-type
                :initform :simulation) ;can be either :simulation or :frame (FIXME make it an "enumerated type" somehow)
   (priority :accessor priority
             :initform +default-system-priority+)))

(defmethod print-object ((object system) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "#~A ~A [~A]" (priority object) (name object) (system-type object))))

(defun find-system (name)
  "Find a system by `NAME'."
  (find name (systems *ecs-manager*) :key #'name))

(defmacro defsystem (name &body required)
  `(progn
     (defclass ,name (system)
       ((name :initform ',name)
        (required :initform ',@required)))
     (when *ecs-manager*
       (if-let ((s (find-system ',name)))
         (setf (required s) ',@required)
         (setf (systems *ecs-manager*)
               (merge 'list
                      (systems *ecs-manager*)
                      (list (make-instance ',name))
                      #'<
                      :key #'priority))))))

(defun register-system (name &key (priority +default-system-priority+) (type :simulation))
  (let ((new (make-instance name)))
    (if-let ((s (find-system name)))
      (progn (setf (required s) (required new)
                   (priority s) priority
                   (system-type s) type)
             (setf (systems *ecs-manager*) (stable-sort (systems *ecs-manager*) #'< :key #'priority)))
      (progn
        (setf (priority new) priority
              (system-type new) type)
        (setf (systems *ecs-manager*)
              (merge 'list
                     (systems *ecs-manager*)
                     (list new)
                     #'<
                     :key #'priority))))))

(defmethod do-system (system entity dt)
  (log:trace "Default do-system called."))

(defmethod entity-added (system entity)
  "Called when an `ENTITY' is registered with a `SYSTEM'.
This method is called after adding components that are needed by the system."
  (log:trace "Default entity-added called."))

(defmethod entity-removed (system entity)
  "Called when an `ENTITY' is unregistered from a `SYSTEM'.
This method is called AFTER removing components that are needed by the system, so they're NOT available.."
  (log:trace "Default entity-removed called."))
