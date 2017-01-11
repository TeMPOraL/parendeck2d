(in-package #:parendeck2d.ecs)

(defparameter +default-system-priority+ 0)

(defclass system ()
  ((name :reader name)
   (required :accessor required
             :initform nil)
   (entities :accessor entities
             :initform nil)
   (priority :accessor priority
             :initform +default-system-priority+)))

(defmethod print-object ((object system) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "#~A ~A" (priority object) (name object))))

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

(defun register-system (name &key (priority +default-system-priority+))
  (let ((new (make-instance name)))
    (if-let ((s (find-system name)))
      (progn (setf (required s) (required new)
                   (priority s) priority)
             (setf (systems *ecs-manager*) (stable-sort (systems *ecs-manager*) #'< :key #'priority)))
      (progn
        (setf (priority new) priority)
        (setf (systems *ecs-manager*)
              (merge 'list
                     (systems *ecs-manager*)
                     (list new)
                     #'<
                     :key #'priority))))))

(defmethod do-system (system entity dt)
  (log:trace "Default do-system called."))
