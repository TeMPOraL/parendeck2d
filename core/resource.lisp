(in-package #:parendeck2d)

;;; NOTE The resources API is in total flux and will likely get redesigned soon.

(defclass resource ()
  ((name :initarg :name
         :accessor name
         :initform (error "must specify resource name"))
   (loaded :reader loadedp
           :initform nil)
   (resource-type :initarg :resource-type
                  :reader resource-type
                  :initform (error "must specify resource type"))))

(defgeneric unload-resource (resource)
  (:documentation "Call this to unload a resource."))
