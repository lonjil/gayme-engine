
;;;; game.lispa

(in-package #:game)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (boundp '+fpi+)
    (defconstant +fpi+ #.(coerce pi 'single-float))))

;;; "game" goes here. Hacks and glory await!

(defvar *array* nil) ; vertices of model
(defvar *stream* nil) ; stream for drawing the models
(defvar *running* nil) ; are we running?
(defvar *index* nil) ; maps vertices to triangles
(defvar *tex* nil) ; model texture (dirt:load-blah "test.jpg")
(defvar *sam* nil) ; (sample *tex*)
(defvar *sphere-vert* nil)
(defvar *sphere-index* nil)
(defvar *sphere* nil)

(defvar *world* nil)
(defvar *floor* 0.0f0)
                                        ;(world-step)

(defvar *ssaa-tex* nil)
(defvar *ssaa-fbo* nil)
(defvar *ssaa-sam* nil)
(defvar *ssaa-stream* nil) ; :primitive :points
(defvar *geometry* nil)

(defmacro get-cached-or ((key table) form-to-cache)
  `(or (gethash ,key ,table)
       (setf (gethash ,key ,table) ,form-to-cache)))
(defmacro laziness (key mesh-form)
  `(get-cached-or (,key *streams*)
                  (bind (((vert index)
                          ,mesh-form))
                    (make-buffer-stream (list vert) :index-array index))))
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
  ((%pos :accessor pos :initarg :pos :type (or vec3 null))
   (%rot :accessor rot :initarg :rot :type (or quat null)))
  (:default-initargs
   :pos (v3:make 0 0 0)
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
(defclass object (entity)
  ((%mass :accessor mass :initarg :mass :type real)
   (%momentum :accessor momentum :initarg :momentum :type vec3)
   (%size :accessor size :initarg :size :type real)
   (%kind :accessor kind :initarg :kind :type (member :geometry :object))
   (%shape :accessor shape :initarg :shape :type (member :cube :plane)))
  (:default-initargs
   :mass 1.0f0
   :momentum (v3:make 0.0f0 0.0f0 0.0f0)
   :size 1.0f0
   :shape :cube
   :kind :object)
  (:documentation "An object in the physics engine."))

(defclass objing (object thing)
  ())

(defclass player (object)
  ((%camera :accessor camera :initarg :camera :type (or camera null))
   (%model :accessor model :initarg :model :type (or thing null))
   (%yaw :accessor yaw :initarg :yaw :type real)
   (%pitch :accessor pitch :initarg :pitch :type real))
  (:default-initargs
   :camera (make-instance 'camera :pos (v3:make 0.0f0 1.0f0 0.0f0))
   :model (make-instance 'thing :sampler *sam* :stream (box-stream))
   :yaw 0.0f0
   :pitch 0.0f0))

(defun make-thing (stream sampler)
  (make-instance 'thing :stream stream :sampler sampler))

(defvar *camera* (make-instance 'camera))
(defvar *camera2* (make-instance 'camera))
(defvar *current-camera* *camera*)
(defvar *things* nil)
(defvar *light-pos* (v3:make 0 30 -5))

(defvar *delta* 1)
(defvar *fps* 0)
(defvar *fps-wip* 0)
(defvar *stepper* (make-stepper (seconds 1)))


(defvar *player* (make-instance 'player))
(defvar *p1* *player*)
(defvar *p2* (make-instance 'player))


(defun-g vert ((vert g-pnt)
               &uniform
               (model->world :mat4)
               (world->view :mat4)
               (view->clip :mat4))
  (let* ((model-pos (v4:make (pos vert) 1.0))
         (normal (norm vert))
         (world-pos (* model->world model-pos))
         (normal (.xyz (* model->world
                          (v4:make normal 1.0))))
         (view-pos (* world->view world-pos))
         (clip-pos (* view->clip view-pos)))
    (values clip-pos
            (tex vert)
            (.xyz world-pos)
            normal)))

(defun-g frag ((uv :vec2)
               (frag-pos :vec3)
               (frag-norm :vec3)
               &uniform
               (sam :sampler-2d)
               (light-pos :vec3)
               (cam-pos :vec3))
  (let* ((object-color (vec4 0 0 0 0))
         (frag-norm (normalize frag-norm))
         (ambient 0.3)

         (dir-to-light (normalize (- light-pos frag-pos)))
         (diffuse (saturate (dot dir-to-light frag-norm)))

         (dir-to-cam (normalize (- cam-pos frag-pos)))
         (reflection (normalize (reflect (- dir-to-light)
                                         frag-norm)))
         (specular (* (expt (saturate (dot reflection dir-to-cam))
                            32)
                      2.0))

         (light (+ ambient
                   diffuse
                   specular)))
    (* light (texture sam uv))))


(defun v4parallelp (a b)
  (= (* (v4:dot a a) (v4:dot b b)) (expt (v4:dot a b) 2)))


(defun smallest ()
  (loop for x in *things*
        for y = x then y
        when (< (scale x)
                (scale y))
          do (setf y x)
        finally (return y)))


(defpipeline-g pipeline ()
  (vert g-pnt)
  (frag :vec2 :vec3 :vec3))

(defun-g ssaa-frag ((uv :vec2)
                    &uniform (scene :sampler-2d))
  (texture scene uv))
(defpipeline-g ssaa-pipe (:points)
  :fragment (ssaa-frag :vec2))


(defun-g crosshair-vert ((vert :vec2)
                         &uniform
                         (ratio :vec2)
                         (size :float))
  (let* ((pos (/ (* vert size)
                 ratio)))
    (v4:make pos -1 1)))
(defun-g crosshair-frag2 ()
  (v4:make 1 0 0 0))

(defpipeline-g crosshair ()
  (crosshair-vert :vec2)
  (crosshair-frag2))

(defun tick ()
  (float (get-internal-real-time)))
(defun world->view (&optional (camera *camera*))
  (m4:* (q:to-mat4 (q:inverse (rot camera)))
        (m4:translation-from-vec3! (m4:id) (v3:negate (pos camera)))))
(defun model->world (thing)
  (m4:* (m4:translation-from-vec3! (m4:id) (pos thing))
        (m4:* (q:to-mat4 (rot thing))
              (m4:scale-from-vec3! (m4:id) (v3:scale (v3:make 1 1 1)
                                                     (scale thing))))))

(defvar *turbo* 10)
(defvar *pitch* 0)
(defvar *yaw* 0)
(defvar *down* nil)
(defvar *active* nil)
(defvar *mpos* (v2:make 0 0))
(defvar *mdelta* (v2:make 0 0))

(defmacro with-accessors* (accessors instance &body body)
  `(with-accessors ,(loop :for x :in accessors :collect (list x x))
       ,instance
     ,@body))

(defun set-draw-params (&optional *camera*)
  (map-g #'pipeline nil
         :light-pos *light-pos*
         :cam-pos (pos *camera*)
         :world->view (world->view *camera*)
         :view->clip ;(m4ortho -40.0f0 40.0f0 -40.0f0 40.0f0 0.1f0 300.0f0)
                                        ;#+(or)
         (m4:perspective-projection
          (fovy *camera*)
          (/ (.x (viewport-resolution (current-viewport)))
             (.y (viewport-resolution (current-viewport))))
          0.2
          3000f0)))
(defgeneric draw (foo))
(defmethod draw ((foo thing))
  (map-g #'pipeline (buf-stream foo)
         :model->world (model->world foo)))

(defvar *dt* 0.1f0)
(defgeneric update (thing &optional *dt*)
  (:documentation "Do all the stuff"))
(defmethod update ((object object) &optional (*dt* *dt*)))
(defmethod update :after ((player player) &optional (*dt* *dt*))
  (with-accessors* (model camera pos rot pitch momentum)
      player
    (v3:+! pos
          pos
          (v3:scale momentum *dt*))
    (setf (rot camera) rot
          (pos camera) (v3:+ pos (v3:make 0.0f0 1f0 0f0))
          (rot model) rot
          (pos model) pos)))
(defgeneric apply-input (thing)
  (:documentation "Update thing according to current inputs."))
(defmethod apply-input ((player player))
  (with-accessors* (yaw pitch momentum rot)
      player
    (when *mouse-captured*
      (setf yaw (mod (+ yaw (* -0.01 (.x *mdelta*)))
                     (* 2 +fpi+))
            pitch (clamp (+ pitch (* -0.01 (.y *mdelta*)))
                         (/ +fpi+ -2)
                         (/ +fpi+ 2)))
      (q:*! rot
            (q:rotate (q:id) (v3:make 0 yaw 0))
            (q:rotate (q:id) (v3:make pitch 0 0))))
    (let* ((mult (coerce *turbo* 'single-float))
           (dir (q:to-vec3 rot))
           (forward (q:to-vec3 (q:rotate (q:id) (v3:make 0 yaw 0))))
           (right (q:to-vec3 (q:rotate (q:id) (v3:make 0
                                                       (+ yaw (radians 90))
                                                       0))))
           (up (v3:make 0 1 0))
           (final (v3:make 0 0 0)))
      (when (keyboard-button (keyboard) key.e)
        (v3:+! final final forward))
      (when (keyboard-button (keyboard) key.f)
        (v3:-! final final forward))
      (when (keyboard-button (keyboard) key.d)
        (v3:+! final final right))
      (when (keyboard-button (keyboard) key.a)
        (v3:-! final final right))
      (when (keyboard-button (keyboard) key.space)
        (v3:+! final final up))
      (when (keyboard-button (keyboard) key.lctrl)
        (v3:-! final final up))
      (when (keyboard-button (keyboard) key.w)
        (v3:+! final final dir))
      (when (keyboard-button (keyboard) key.s)
        (v3:-! final final dir))
      (setf momentum (v3:scale (v3:normalize final) mult))
                                        ;(v3+! (pos camera) (pos camera) (v3scale (v3normalize final) mult))
      (when (keyboard-button (keyboard) key.r)
        (setf rot (q:make 1 0 0 0)
              yaw 0f0
              pitch 0f0)))))



(defun update-camera (camera)
  (when *mouse-captured*
    (setf *yaw* (float (mod (+ *yaw* (* -0.01 (.x *mdelta*)))
                            (* 2 +fpi+))
                       1.0f0)
          *pitch* (float (clamp (+ *pitch* (* -0.01 (.y *mdelta*)))
                                (/ +fpi+ -2)
                                (/ +fpi+ 2))
                         1.0f0))
    (q:*! (rot camera)
         (q:rotate (q:id) (v3:make 0 *yaw* 0))
         (q:rotate (q:id) (v3:make *pitch* 0 0))))
  (let* ((mult (* *turbo* *delta*))
         (dir (q:to-vec3 (rot camera)))
         (forward (q:to-vec3 (q:rotate (q:id) (v3:make 0 *yaw* 0))))
         (right (q:to-vec3 (q:rotate (q:id) (v3:make 0
                                                     (+ *yaw* (radians 90))
                                                     0))))
         (up (v3:make 0 1 0))
         (final (v3:make 0 0 0)))
    (when (keyboard-button (keyboard) key.e)
      (v3:+! final final forward))
    (when (keyboard-button (keyboard) key.f)
      (v3:-! final final forward))
    (when (keyboard-button (keyboard) key.d)
      (v3:+! final final right))
    (when (keyboard-button (keyboard) key.a)
      (v3:-! final final right))
    (when (keyboard-button (keyboard) key.space)
      (v3:+! final final up))
    (when (keyboard-button (keyboard) key.lctrl)
      (v3:-! final final up))
    (when (keyboard-button (keyboard) key.w)
      (v3:+! final final dir))
    (when (keyboard-button (keyboard) key.s)
      (v3:-! final final dir))
    (v3:+! (pos camera) (pos camera) (v3:scale (v3:normalize final) mult))
    (when (keyboard-button (keyboard) key.r)
      (setf (rot camera) (v4:make 1 0 0 0)
            *yaw* 0f0
            *pitch* 0f0))))

(defun update-mouse ()
  (let ((down (mouse-button (mouse) 1)))
    (setf *active* (and down *down*))
    (setf *down* down))
  (let ((new-pos (mouse-pos (mouse))))
    (v2:-! *mdelta* new-pos *mpos*)
    (setf *mpos* new-pos)))

(define-symbol-macro xdim (resolution (current-viewport)))
(defun ilist (vec) (loop for e across vec collect (floor e)))
(define-symbol-macro xdiml (ilist xdim))
(defun blit-setup (&optional (scale 1.0)
                   &aux (dim (ilist (v2:scale xdim scale))))
  (setf *ssaa-tex* (make-texture nil :dimensions dim
                                     :element-type :vec4)
        *ssaa-sam* (sample *ssaa-tex*)
        *ssaa-fbo* (make-fbo (list 0 *ssaa-tex*)
                             (list :d :dimensions dim))
        *ssaa-stream* (make-buffer-stream nil :primitive :points)))
(defun blit-kill ()
  (free *ssaa-fbo*)
  (free *ssaa-sam*)
  (free *ssaa-tex*)
  (free *ssaa-stream*))

(defvar *pick-fbo* nil)
(defun pick-setup (&optional (scale 1.0)
                   &aux (dim (ilist (v2:scale xdim scale))))
  (setf *ssaa-fbo* (make-fbo (list 0 :dimensions dim)
                             (list :d :dimensions dim))))
(defun pick-kill ()
  (free *ssaa-fbo*))

(defun-g picking-vert ((vert g-pnt)
                       &uniform
                       (model->world :mat4)
                       (world->view :mat4)
                       (view->clip :mat4))
  (let* ((model-pos (v4:make (pos vert) 1.0))
         (normal (norm vert))
         (world-pos (* model->world model-pos))
         (normal (.xyz (* model->world
                          (v4:make normal 1.0f0))))
         (view-pos (* world->view world-pos))
         (clip-pos (* view->clip view-pos)))
    clip-pos))
(defun-g picking-frag (&uniform
                       (picking-color :vec3))
  (v4:make picking-color 1))
(defpipeline-g picking-prog ()
  (picking-vert g-pnt)
  (picking-frag))

(defun id-to-color (num)
  (v3:make (/ (ldb (byte 8 0) num) 255.0f0)
           (/ (ldb (byte 8 8) num) 255.0f0)
           (/ (ldb (byte 8 16) num) 255.0f0)))
(defun color-to-id (color)
  (logior (dpb (.x color) (byte 8 0) 0)
          (dpb (.y color) (byte 8 8) 0)
          (dpb (.z color) (byte 8 16) 0)))

(defun get-object-under-crosshair ()
  (let ((bgcolor (clear-color))
        (w (.x (viewport-resolution (current-viewport))))
        (h (.y (viewport-resolution (current-viewport)))))
    (setf (clear-color) (v4:make 1 1 1 1))
    (clear)
    (map-g #'picking-prog nil
           :world->view (world->view (camera *player*))
           :view->clip (m4:perspective-projection
                        (radians 60f0)
                        (/ w h)
                        0.2
                        300f0))
    (loop :for x :in *things*
          :for i :from 0
          :do (map-g #'picking-prog (buf-stream x)
                     :model->world (model->world x)
                     :picking-color (id-to-color i)))
    (setf (clear-color) bgcolor)
    (let ((id (color-to-id (gl:read-pixels (/ w 2) (/ h 2)
                                           1 1
                                           :rgb :unsigned-byte))))
      (if (= id 16777215)
          nil
          (nth id *things*)))))
(defun-g line-vert ((vert :vec3)
                    &uniform
                    (model->world :mat4)
                    (world->view :mat4)
                    (view->clip :mat4)
                    (length :float))
  (let* ((pos (v4:make vert (/ 1.0f0 length)))
         (pos (* model->world pos))
         (pos (* world->view pos))
         (pos (* view->clip pos)))
    pos))
(defun-g line-frag ()
  (v4:make 1 0 0 0))

(defpipeline-g line-prog (:lines)
  (line-vert :vec3)
  (line-frag))

(defvar *under-crosshair* nil)
(defvar *mouse-captured* nil)

(defun step-demo ()
  (declare (optimize (debug 0)))
  (update-repl-link)

  (incf *fps-wip*)
  (when (funcall *stepper*)
    (setf *fps* *fps-wip*
          *fps-wip* 0))
  (setf *delta* (/ 1.0 *fps*))

  (setf *under-crosshair* (get-object-under-crosshair))

  (when *under-crosshair*
    (when (keyboard-button (keyboard) key.kp_plus)
      (setf (scale *under-crosshair*)
            (* (scale *under-crosshair*) 1.1f0)))
    (when (keyboard-button (keyboard) key.kp_minus)
      (setf (scale *under-crosshair*)
            (/ (scale *under-crosshair*)
               1.1f0)))
    (when (keyboard-button (keyboard) key.p)
      (setf (buf-stream *under-crosshair*) *sphere*))
    (when (keyboard-button (keyboard) key.o)
      (setf (buf-stream *under-crosshair*) *stream*)))

  (when (keyboard-button (keyboard) key.n)
    (format t "what~%")
    (let ((obj (make-thing *stream* *sam*)))
      (setf (pos obj) (v3:+ (v3:scale (q:to-vec3 (rot *current-camera*))
                                      -5.0f0)
                            (pos *current-camera*)))
      (push obj *things*)))

  (when (keyboard-button (keyboard) key.q)
    (sdl2-ffi.functions:sdl-capture-mouse 0)
    (sdl2-ffi.functions:sdl-show-cursor 1)
    (setf *mouse-captured* nil))
  (when (keyboard-button (keyboard) key.escape)
    (sdl2-ffi.functions:sdl-capture-mouse 1)
    (sdl2-ffi.functions:sdl-show-cursor 0)
    (setf *mouse-captured* t))

  (step-host)
  (update-mouse)
  (when (keyboard-button (keyboard) key.lshift)
    (setf *turbo* 100))
  (when (keyboard-button (keyboard) key.lalt)
    (setf *turbo* 1))
  (when (keyboard-button (keyboard) key.ralt)
    (setf *turbo* 0.0001f0))
  (apply-input *player*)
  (update *player*)
  (when (and *down* (not *active*))
    (format t "~a~%" (rot *player*)))

  (setf (resolution (current-viewport))
        (surface-resolution (current-surface (cepl-context))))
  (clear)
  (set-draw-params (camera *player*))
  (clear-fbo *ssaa-fbo*)
  (draw (model *p1*))
  (draw (model *p2*))
  #+(or)(loop :for x :in *geometry*
              :do (draw x))
  (loop :for x :in *things* :do
    (draw x))
  (with-fbo-bound (*ssaa-fbo* :with-blending nil)
    #+(or)(map-g #'line-prog *line*
                 :model->world (m4invt (world->view *camera*))
                 :world->view (world->view *current-camera*)
                 :view->clip (m4persp
                              (radians 60f0)
                              (/ (.x (viewport-resolution (current-viewport)))
                                 (.y (viewport-resolution (current-viewport))))
                              0.2
                              300f0)
                 :length 200.0f0))
  #+(or)(map-g #'ssaa-pipe *ssaa-stream*
         :scene *ssaa-sam*)
  #+(or)(map-g #'crosshair *tri-stream*
         :ratio (viewport-resolution (current-viewport))
         :size 100.0f0)
  ;(nineveh:draw-tex-br *sam*)
  (swap)
  (decay-events)
  (setf *turbo* 10))

(defun dist (things)
  (loop :for thing :in things :do
    (setf (pos thing)
          (v3:+ (v3:make 0 0 -10)
                (v3:make (- (random 100) 50)
                         (random 100)
                         (- (random 10) 5))))))
(defun rote (things)
  (loop :for thing :in things :do
    (setf (rot thing)
          (q:from-vec3 ;q:from-fixed-angles-v3
           (v3:make (- (random 20f0) 10)
                    (random 60f0)
                    (- (random 10f0) 10))))))
(defun update-thing (thing)
  (with-slots (pos) thing
    #+(or)(setf (.y pos) (mod (- (.y pos) (* *delta* 0))
                              100f0))))

(defun run-loop ()
  (when *running* (return-from run-loop))
  (setf *running* t)
  (slynk-mrepl::send-prompt (find (bt:current-thread)
                                  (slynk::channels)
                                  :key #'slynk::channel-thread))
  (unwind-protect
       (loop :while (and *running* (not (shutting-down-p))) :do
         (continuable (step-demo)))
    (setf *running* nil)))
(defun stop-loop ()
  (setf *running* nil))

(defun init ()
  (when *stream*
    (free *stream*))
  (when *array*
    (free *array*))
  (when *index*
    (free *index*))
  (when *sphere*
    (free *sphere*))
  (bind (((vert index)
          (nineveh.mesh.data.primitives:cube-gpu-arrays))
         ((svert sind)
          (nineveh.mesh.data.primitives:sphere-gpu-arrays)))
    (setf *array* vert
          *index* index
          *stream* (make-buffer-stream (list vert)
                                       :index-array index)
          *sphere* (make-buffer-stream (list svert)
                                       :index-array sind))))
(defvar *tri* nil)
(defvar *tri-index* nil)
(defvar *tri-stream* nil)
(defun init-crosshair ()
  (when *tri*
    (free *tri*))
  (when *tri-index*
    (free *tri-index*))
  (when *tri-stream*
    (free *tri-stream*))
  (setf *tri* (make-gpu-array (list (v2:make 0.0f0  0.0f0)
                                    (v2:make 0.0f0 -0.5f0)
                                    (v2:make 0.5f0  0.0f0))
                              :element-type :vec2)
        *tri-index* (make-gpu-array (list 0 1 2))
        *tri-stream* (make-buffer-stream *tri*
                                         :index-array *tri-index*)))
(defun awoo ()
  (when *sam* (free *sam*))
  (when *tex* (free *tex*))
  (setf *tex* (dirt:load-image-to-texture "./awoo.jpg")
        *sam* (sample *tex*)))

                                        ;(run-loop)
                                        ;aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
