(in-package #:lafps)

(defvar *light*)

(defclass light (entity)
  ((%scale :accessor scale :initarg :scale)
   (%stream :accessor buf-stream :initarg :stream)
   (%color :accessor color :initarg :color))
  (:default-initargs :scale 1.0f0)
  (:documentation "A light"))
(defun init-misc ()
  (setf *light* (make-instance 'light
                               :color (v3:vec 1 1 1) :stream *stream*
                               :pos (v3:vec 0 0 15))))
(defmethod draw ((light light))
  (c:map-g #'lamp (buf-stream light)
           :obj (model->world light)))
