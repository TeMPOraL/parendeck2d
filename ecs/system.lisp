(in-package #:parendeck2d.ecs)

(defclass system ()
  ((name :reader name)
   (required :accessor required
             :initform nil)
   (entities :accessor entities
             :initform nil)))

(defmethod print-object ((object system) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~A" (name object))))

(defun find-system (name)
  "Find a system by `NAME'."
  (find name (systems *ecs-manager*) :key #'name))

(defmacro defsystem (name &body required)
  `(progn
     (defclass ,name (system)
       ((name :initform ',name)
        (required :initform ',@required)))
     (if-let ((s (find-system ',name)))
       (setf (required s) ',@required)
       (appendf (systems *ecs-manager*) (list (make-instance ',name))))))

(defmethod do-system (system entity))
