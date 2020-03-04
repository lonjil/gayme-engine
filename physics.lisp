(in-package #:lafps)

(defvar *dt* 0.1)
(defgeneric update (thing &optional *dt*)
  (:documentation "Do all the stuff"))

(defclass object (entity)
  ((%mass :accessor mass :initarg :mass :type real)
   (%momentum :accessor momentum :initarg :momentum :type vec3)
   (%size :accessor size :initarg :size :type real)
   (%kind :accessor kind :initarg :kind :type (member :geometry :object))
   (%shape :accessor shape :initarg :shape :type (member :cube :plane)))
  (:default-initargs
   :mass 1.0f0
   :momentum (v3:vec)
   :size 1.0f0
   :shape :cube
   :kind :object)
  (:documentation "An object in the physics engine."))

