
;;;; game.lispa

(in-package #:lafps)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (boundp '+fpi+)
    (defconstant +fpi+ #.(coerce pi 'single-float))))

;;; "game" goes here. Hacks and glory await!

(defvar *running* nil) ; are we running?
(defvar *tex* nil) ; model texture (dirt:load-blah "test.jpg")
(defvar *sam* nil) ; (sample *tex*)

(defvar *floor* 0.0f0)
                                        ;(world-step)

(defvar *stream* nil)
(defvar *index* nil)
(defvar *array* nil)

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

(defclass entity ()
  ((%pos :accessor pos :initarg :pos :type (or v3:vec null))
   (%rot :accessor rot :initarg :rot :type (or q:quat null)))
  (:default-initargs
   :pos (v3:zero)
   :rot (q:id))
  (:documentation "Anything in the worldspace has this as a superclass"))

(defclass camera (entity)
  ((%fovy :accessor fovy :initarg :fovy :type real)
   (%zoom :accessor zoom :initarg :zoom :type real)
   (%mode :accessor mode :initarg :mode
          :type (member :perspective :orthographic)))
  (:default-initargs
   :fovy (radians 75)
   :zoom 1
   :mode :perspective)
  (:documentation "\"This is not documentation\""))
(defclass thing (entity)
  ((%scale :accessor scale :initarg :scale :type real)
   (%stream :accessor buf-stream :initarg :stream
            :type (or buffer-stream null))
   (%sampler :accessor sampler :initarg :sampler
             :type (or sampler null)))
  (:default-initargs
   :scale 1.0f0)
  (:documentation "A world object that can be drawn"))

(defvar *camera* (make-instance 'camera))
(defvar *camera2* (make-instance 'camera))
(defvar *current-camera* *camera*)

(defvar *delta* 1)
(defvar *fps* 0)
(defvar *fps-wip* 0)
(defvar *stepper* (t:make-stepper (t:seconds 1)))

(c:defun-g vert ((vert c:g-pnt)
               &uniform
               (model->world :mat4)
               (world->view :mat4)
               (view->clip :mat4))
  (let* ((model-pos (vari:vec4 (c:pos vert) 1.0))
         (normal (c:norm vert))
         (world-pos (* model->world model-pos))
         (normal (vari:swizzle (* model->world (vari:vec4 normal 1.0))
                               :xyz))
         (view-pos (* world->view world-pos))
         (clip-pos (* view->clip view-pos)))
    (values clip-pos
            (c:tex vert)
            (vari:swizzle world-pos :xyz)
            normal)))

(c:defun-g frag ((uv :vec2)
                 (frag-pos :vec3)
                 (frag-norm :vec3)
                 &uniform
                 (sam :sampler-2d)
                 (light-pos :vec3)
                 (cam-pos :vec3))
  (let* ((object-color (vari:vec4 0 0 0 0))
         (frag-norm (vari:normalize frag-norm))
         (ambient 0.7)

         (dir-to-light (vari:normalize (- light-pos frag-pos)))
         (diffuse (rtg-math:saturate (vari:dot dir-to-light frag-norm)))

         (dir-to-cam (vari:normalize (- cam-pos frag-pos)))
         (reflection (vari:normalize (vari:reflect (- dir-to-light)
                                                   frag-norm)))
         (specular (* (expt (rtg-math:saturate (vari:dot reflection dir-to-cam))
                            32)
                      2.0))

         (light (+ ambient
                   diffuse
                   specular)))
    (* light (c:texture sam uv))))


(defun v4parallelp (a b)
  (= (* (v4:dot a a) (v4:dot b b)) (expt (v4:dot a b) 2)))


(c:defpipeline-g pipeline ()
  (vert c:g-pnt)
  (frag :vec2 :vec3 :vec3))


(defun tick ()
  (float (get-internal-real-time)))

(defun world->view (&optional (camera *camera*))
  (m4:* (q:to-mat4 (q:inverse (rot camera)))
        (m4:set-translation (m4:id) (v3:negate (pos camera)))))
(defun model->world (thing)
  (m4:* (m4:set-translation (m4:id) (pos thing))
        (m4:* (q:to-mat4 (rot thing))
              (m4:set-scale (m4:id) (v3:scale (v3:make 1 1 1)
                                                     (scale thing))))))

(defvar *turbo* 0)
(defvar *pitch* 0)
(defvar *yaw* 0)
(defvar *down* nil)
(defvar *active* nil)
(defvar *mpos* (v2:make 0 0))
(defvar *mdelta* (v2:make 0 0))

(defun rotate-vec3! (out vec quat)
  (let* ((r (q:from-vec3 vec))
         (q-conj (q:conjugate quat)))
    (q:to-vec3! out (q:* (q:* quat r) q-conj))))
(defun rotate-vec3 (vec quat)
  (rotate-vec3! (v3:zero) vec quat))
(defun q2ray! (out-vec quat)
  (rotate-vec3! out-vec (v3:make 0 0 1) quat))
(defun q2ray (quat)
  (q2ray! (v3:zero) quat))

(defmacro with-accessors* (accessors instance &body body)
  `(with-accessors ,(loop :for x :in accessors :collect (list x x))
       ,instance
     ,@body))

(defun set-draw-params (&optional *camera*)
  (c:map-g #'pipeline nil
           :cam-pos (pos *camera*)
           :world->view (world->view *camera*)
           :view->clip (m4:set-projection/perspective
                        (fovy *camera*)
                        (/ (c:viewport-resolution-x (c:current-viewport))
                           (c:viewport-resolution-y (c:current-viewport)))
                        0.2
                        3000f0)))
(defgeneric draw (foo))

(defvar *dt* 0.1f0)
(defun clamp (min max val)
  (max min (min (min max val))))
(defun apply-input ()
  (when *mouse-captured*
    (setf *yaw* (mod (+ *yaw* (* -0.01 (v2:x *mdelta*)))
                     (* 2 +fpi+))
          *pitch* (clamp (/ +fpi+ -2)
                       (/ +fpi+ 2)
                       (+ pitch (* -0.01 (v2:y *mdelta*)))))
    (q:*! rot
          (q:rotate (q:id) (v3:make 0 *yaw* 0))
          (q:rotate (q:id) (v3:make *pitch* 0 0))))
  (let* ((mult (* *delta* (coerce *turbo* 'single-float) 50))
         (dir (v3:negate (q2ray rot)))
         (forward (v3:negate (q2ray (q:rotate (q:id) (v3:make 0 yaw 0)))))
         (right (q2ray (q:rotate (q:id) (v3:make 0
                                                 (+ *yaw* (radians 90))
                                                 0))))
         (up (v3:make 0 1 0))
         (final (v3:make 0 0 0))
         (keyb (s:keyboard)))
    (when (s:keyboard-button keyb k:key.e)
      (v3:+! final final forward))
    (when (s:keyboard-button keyb k:key.f)
      (v3:-! final final forward))
    (when (s:keyboard-button keyb k:key.d)
      (v3:+! final final right))
    (when (s:keyboard-button keyb k:key.a)
      (v3:-! final final right))
    (when (s:keyboard-button keyb k:key.space)
      (v3:+! final final up))
    (when (s:keyboard-button keyb k:key.lctrl)
      (v3:-! final final up))
    (when (s:keyboard-button keyb k:key.w)
      (v3:+! final final dir))
    (when (s:keyboard-button keyb k:key.s)
      (v3:-! final final dir))
    ;;(setf momentum (v3:scale (v3:normalize final) mult))
    ;;(v3+! (pos camera) (pos camera) (v3scale (v3normalize final) mult))
    (when (s:keyboard-button keyb k:key.r)
      (princ "h")
      (setf ;;rot (q:make 1 0 0 0)
            yaw 0f0
            pitch 0f0))))


(defun update-mouse ()
  (let ((down (s:mouse-button (s:mouse) 1)))
    (setf *active* (and down *down*))
    (setf *down* down))
  (let ((new-pos (s:mouse-pos (s:mouse))))
    (v2:-! *mdelta* new-pos *mpos*)
    (setf *mpos* new-pos)))


(defvar *mouse-captured* nil)

(defun step-demo ()
  (declare (optimize (debug 3)))
  (livesupport:update-repl-link)

  (incf *fps-wip*)
  (when (funcall *stepper*)
    (setf *fps* *fps-wip*
          *fps-wip* 0))
  (setf *delta* (/ 1.0 *fps*))

  (when (s:keyboard-button (s:keyboard) k:key.q)
    (sdl2-ffi.functions:sdl-capture-mouse 0)
    (sdl2-ffi.functions:sdl-show-cursor 1)
    (setf *mouse-captured* nil))
  (when (s:keyboard-button (s:keyboard) k:key.escape)
    (sdl2-ffi.functions:sdl-capture-mouse 1)
    (sdl2-ffi.functions:sdl-show-cursor 0)
    (setf *mouse-captured* t))

  (c:step-host)
  (update-mouse)

  (setf (c:resolution (c:current-viewport))
        (c:surface-resolution (c:current-surface (c:cepl-context))))
  (c:clear)
  ;;(set-draw-params (camera *player*))
  (loop :for x :in *things* :do
    (draw x))
  (c:swap)
  (s:decay-events))

(defun run-loop ()
  (when *running* (return-from run-loop))
  (setf *running* t)
  (slynk-mrepl::send-prompt (find (bt:current-thread)
                                  (slynk::channels)
                                  :key #'slynk::channel-thread))
  (unwind-protect
       (loop :while (and *running* (not (c:shutting-down-p))) :do
         (livesupport:continuable (step-demo)))
    (setf *running* nil)))
(defun stop-loop ()
  (setf *running* nil))

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
