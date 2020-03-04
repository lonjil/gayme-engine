(in-package #:lafps)

(defclass entity ()
  ((%pos :accessor pos :initarg :pos :type v3:vec)
   (%rot :accessor rot :initarg :rot :type q:quat))
  (:default-initargs
   :pos (v3:vec)
   :rot (q:id! (q:quat)))
  (:documentation
   "Anything with a coordinate in the worldspace has this as a superclass"))
