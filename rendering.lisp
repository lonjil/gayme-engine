(in-package #:lafps)


(defvar *tex* nil) ; model texture (dirt:load-blah "test.jpg")
(defvar *sam* nil) ; (sample *tex*)

(defvar *floor* 0.0f0)
                                        ;(world-step)

(defvar *stream* nil)
(defvar *index* nil)
(defvar *array* nil)
(defvar *sphere* nil)

(defmacro get-cached-or ((key table) form-to-cache)
  `(or (gethash ,key ,table)
       (setf (gethash ,key ,table) ,form-to-cache)))
(defmacro laziness (key mesh-form)
  `(get-cached-or
       (,key *streams*)
       (let ((shit ,mesh-form))
         (c:make-buffer-stream (first shit) :index-array (second shit)))))
(defvar *streams* (make-hash-table :test 'equal))
(defun cube-stream (&key (size 1.0f0))
  (laziness (list 'cube size)
            (nineveh.mesh.data.primitives:cube-gpu-arrays :size size)))
(defun plane-stream (&key (width 1.0f0) (height 1.0f0))
  (laziness (list 'plane width height)
            (nineveh.mesh.data.primitives:plain-gpu-arrays :width width
                                                           :height height)))
(defun sphere-stream (&key (radius 0.5f0))
  (laziness (list 'sphere radius)
            (nineveh.mesh.data.primitives:sphere-gpu-arrays :radius radius)))
(defun box-stream (&key (width 1.0f0) (height 1.0f0) (depth 1.0f0))
  (laziness (list 'box width height depth)
            (nineveh.mesh.data.primitives:box-gpu-arrays :width width
                                                         :height height
                                                         :depth depth)))



(defclass thing (entity)
  ((%scale :accessor scale :initarg :scale :type real)
   (%stream :accessor buf-stream :initarg :stream
            :type (or buffer-stream null))
   (%sampler :accessor sampler :initarg :sampler
             :type (or sampler null)))
  (:default-initargs
   :scale 1.0f0)
  (:documentation "A world object that can be drawn"))

(defvar *things* nil)
(defun populate (&optional (c 100))
  (loop :repeat c
        :do (push (make-instance
                   'thing :sampler *sam*
                   :stream (cube-stream)
                   :rot (q:random)
                   :pos (v3:vec (- (random 100) 50)
                                (- (random 100) 50)
                                (- -5 (random 10))))
                  *things*)))

(defvar *camera* (make-instance 'camera))
(defvar *camera2* (make-instance 'camera))
(defvar *current-camera* *camera*)



(defun model->world (thing)
  (m4:* (m4:set-translation m4:+id+ (pos thing))
        (m4:* (q:to-mat4 (rot thing))
              (m4:set-scale m4:+id+ (v3:scale (v3:vec 1 1 1)
                                              (scale thing))))))

(defun set-draw-params (prog &optional *camera*)
  (c:map-g prog nil
           :persp (m4:set-projection/perspective
                   (radians 60.0)
                   (/ (c:viewport-resolution-x (c:current-viewport))
                      (c:viewport-resolution-y (c:current-viewport)))
                   0.01
                   400.0)
           :cam (foop *camera*)
           :light (v3:vec 1 1 1)))
(defgeneric draw (foo))
(defmethod draw ((thing thing))
  (c:map-g #'vert-lit (buf-stream thing)
           :obj (model->world thing)
           :sam (sampler thing)))

(defun render ()
  (declare (optimize (debug 3)))
  (incf *fps-wip*)
  (when (funcall *stepper*)
    (setf *fps* *fps-wip*
          *fps-wip* 0))
  (setf *delta* (/ *fps*))
  (c:step-host)
  (setf (c:resolution (c:current-viewport))
        (c:surface-resolution (c:current-surface (c:cepl-context))))
  (c:clear)
  (set-draw-params #'vert-lit *camera*)
  (set-draw-params #'lamp *camera*)
  #+(or)(c:map-g #'pipe2 *stream*
           :obj m4:+id
           :sam *sam*)
  (loop :for x :in *things* :do (draw x))
  (draw *light*)
  (c:swap))

(defun init ()
  (when *stream*
    (c:free *stream*))
  (when *array*
    (c:free *array*))
  (when *index*
    (c:free *index*))
  (when *sphere*
    (c:free *sphere*))
  (metabang-bind:bind (((vert index)
          (nineveh.mesh.data.primitives:cube-gpu-arrays))
         ((svert sind)
          (nineveh.mesh.data.primitives:sphere-gpu-arrays)))
    (setf *array* vert
          *index* index
          *stream* (c:make-buffer-stream (list vert)
                                       :index-array index)
          *sphere* (c:make-buffer-stream (list svert)
                                       :index-array sind))))
(defun awoo ()
  (when *sam* (c:free *sam*))
  (when *tex* (c:free *tex*))
  (setf *tex* (dirt:load-image-to-texture "./awoo.jpg")
        *sam* (c:sample *tex*)))
