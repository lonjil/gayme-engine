;;;; game.lispa

(in-package #:game)

;;; "game" goes here. Hacks and glory await!

(defvar *array* nil) ; vertices of model
(defvar *stream* nil) ; stream for drawing the models
(defvar *running* nil) ; are we running?
(defvar *index* nil) ; maps vertices to triangles
(defvar *tex* nil) ; model texture (dirt:load-blah "test.jpg")
(defvar *sam* nil) ; (sample *tex*)
(defvar *pos* nil) ; gpu-array of model positions (vec3)
(defvar *rot* nil) ; gpu-array of model rotations (quat)

(defvar *ssaa-tex* nil)
(defvar *ssaa-fbo* nil)
(defvar *ssaa-sam* nil)
(defvar *ssaa-stream* nil) ; :primitive :points

(defclass camera ()
  ((pos :initform (vec3 0 0 0) :accessor pos)
   (rot :initform (qid) :accessor rot)))
(defclass thing ()
  ((pos :initform (vec3 0 0 0) :accessor pos)
   (rot :initform (qid) :accessor rot)))

(defvar *camera* (make-instance 'camera))
(defvar *things* (loop repeat 400
                       collect (make-instance 'thing)))
(defvar *light-pos* (vec3 0 30 -5))

(defvar *delta* 1)
(defvar *fps* 0)
(defvar *fps-wip* 0)
(defvar *stepper* (make-stepper (seconds 1)))


(defun-g vert ((vert g-pnt)
               &uniform
               (model->world :mat4)
               (world->view :mat4)
               (view->clip :mat4))
  (let* ((model-pos (v! (pos vert) 1.0))
         (normal (norm vert))
         (world-pos (* model->world model-pos))
         (normal (* (m4:to-mat3 model->world)
                    normal))
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
         (ambient 0.1)

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


(defpipeline-g pipeline ()
  (vert g-pnt)
  (frag :vec2 :vec3 :vec3))

(defun-g ssaa-frag ((uv :vec2)
                    &uniform (scene :sampler-2d))
  (texture scene uv))
(defpipeline-g ssaa-pipe (:points)
  :fragment (ssaa-frag :vec2))

(defun tick ()
  (float (get-internal-real-time)))
(defun world->view (&optional (camera *camera*))
  (m4* (q->m4 (qinv (rot camera)))
       (v3->m4tr! (m4id) (v3neg (pos camera)))))
(defun model->world (thing)
         (m4* (v3->m4tr! (m4id) (pos thing))
              (m4* (q->m4 (rot thing))
                   (v3->m4scale! (m4id) (v! 3 3 3)))))

(defvar *turbo* 10)
(defvar *pitch* 0)
(defvar *yaw* 0)
(defvar *down* nil)
(defvar *active* nil)
(defvar *mpos* (vec2 0 0))
(defvar *mdelta* (vec2 0 0))
(defun update-camera (camera)
  (when *down*
    (setf *yaw* (float (mod (+ *yaw* (* -0.01 (.x *mdelta*)))
                            (* 2 pi))
                       1.0f0)
          *pitch* (float (clamp (+ *pitch* (* -0.01 (.y *mdelta*)))
                                (/ pi -2)
                                (/ pi 2))
                         1.0f0))
    (q*! (rot camera)
         (qrot (qid) (v! 0 *yaw* 0))
         (qrot (qid) (v! *pitch* 0 0))))
  (let* ((mult (* *turbo* *delta*))
         (dir (m4*v3 (q->m4 (rot camera))
                     (vec3 0 0 -1)))
         (forward (m4*v3 (q->m4 (qrot (qid) (v! 0 *yaw* 0)))
                         (vec3 0 0 -1)))
         (right (m4*v3 (q->m4 (qrot (qid) (vec3 0 (+ *yaw*
                                                     (radians 90))
                                                0)))
                       (vec3 0 0 1)))
         (up (vec3 0 1 0))
         (final (vec3 0 0 0)))
    (when (keyboard-button (keyboard) key.e)
      (v3+! final final forward))
    (when (keyboard-button (keyboard) key.f)
      (v3-! final final forward))
    (when (keyboard-button (keyboard) key.d)
      (v3+! final final right))
    (when (keyboard-button (keyboard) key.a)
      (v3-! final final right))
    (when (keyboard-button (keyboard) key.space)
      (v3+! final final up))
    (when (keyboard-button (keyboard) key.lctrl)
      (v3-! final final up))
    (when (keyboard-button (keyboard) key.w)
      (v3+! final final dir))
    (when (keyboard-button (keyboard) key.s)
      (v3-! final final dir))
    (v3+! (pos camera) (pos camera) (v3scale (v3normalize final) mult))
    (when (keyboard-button (keyboard) key.r)
      (setf (rot *camera*) (v! 1 0 0 0)
            *yaw* 0f0
            *pitch* 0f0))))

(defun update-mouse ()
  (let ((down (mouse-button (mouse) 1)))
    (setf *active* (and down *down*))
    (setf *down* down))
  (let ((new-pos (mouse-pos (mouse))))
    (v2-! *mdelta* new-pos *mpos*)
    (setf *mpos* new-pos)))

(define-symbol-macro xdim (resolution (current-viewport)))
(defun ilist (vec) (loop for e across vec collect (floor e)))
(define-symbol-macro xdiml (ilist xdim))
(defun blit-setup (&optional (scale 1.0)
                   &aux (dim (ilist (v2scale xdim scale))))
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

(defun step-demo ()
  (declare (optimize (debug 3)))
  (update-repl-link)

  (incf *fps-wip*)
  (when (funcall *stepper*)
    (setf *fps* *fps-wip*
          *fps-wip* 0))
  (setf *delta* (/ 1.0 *fps*))

  (step-host)
  (update-mouse)
  (when (keyboard-button (keyboard) key.lshift)
    (setf *turbo* 100))
  (update-camera *camera*)
  (when (and *down* (not *active*))
    (format t "~a~%" (pos *camera*)))
  (setf (resolution (current-viewport))
        (surface-resolution (current-surface (cepl-context))))
  (clear)
  (map-g #'pipeline nil
         :sam *sam*
         :light-pos *light-pos*
         :cam-pos (pos *camera*)
         :world->view (world->view *camera*)
         :view->clip (m4persp
                      (radians 60f0)
                      (/ (.x (viewport-resolution (current-viewport)))
                         (.y (viewport-resolution (current-viewport))))
                      0.2
                      300f0))
  (clear-fbo *ssaa-fbo*)
  (with-fbo-bound (*ssaa-fbo* :with-blending nil)
    (with-instances 400
      (loop :for thing :in *things* :do
        (update-thing thing)
        (map-g #'pipeline *stream*
               :model->world (model->world thing)))))
  (map-g #'ssaa-pipe *ssaa-stream*
         :scene *ssaa-sam*)
  (swap)
  (decay-events)
  (setf *turbo* 10))

(defun dist (things)
  (loop :for thing :in things :do
    (setf (pos thing)
          (v3+ (vec3 0 0 -10)
               (vec3 (- (random 100) 50)
                     (- (random 100) 50)
                     (- (random 10) 5))))))
(defun rote (things)
  (loop :for thing :in things :do
    (setf (rot thing)
          (q:from-fixed-angles-v3
                (v! (- (random 20f0) 10)
                    (random 60f0)
                    (- (random 10f0) 10))))))
(defun update-thing (thing)
  (with-slots (pos) thing
    (setf (.y pos) (mod (- (.y pos) (* *delta* 1))
                        100f0))))

(defun run-loop ()
  (when *running* (return-from run-loop))
  (setf *running* t
        ;*array* (make-gpu-array *atest* :element-type 'pos-col)
        ;*index* (make-gpu-array *testi* :element-type :uint)
        ;*stream* (make-buffer-stream *array* :index-array *index*)
        )
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
  (when *rot*
    (free *rot*))
  (when *pos*
    (free *pos*))
  (bind (((vert index)
          (nineveh.mesh.data.primitives:cube-gpu-arrays)))
    (setf *array* vert
          *index* index
          *pos* (make-gpu-array (loop for x in *things* collect (pos x))
                                :element-type :vec3)
          *rot* (make-gpu-array (loop for x in *things* collect (rot x))
                                :element-type :vec4)
          *stream* (make-buffer-stream (list vert
                                             (cons *pos* 1)
                                             (cons *rot* 1))
                                       :index-array index))))
(defun awoo ()
  (when *sam* (free *sam*))
  (when *tex* (free *tex*))
  (setf *tex* (dirt:load-image-to-texture "./awoo.jpg")
        *sam* (sample *tex*)))

;(run-loop)
