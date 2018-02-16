
;;;; game.lispa

(in-package #:game)

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

(defvar *ssaa-tex* nil)
(defvar *ssaa-fbo* nil)
(defvar *ssaa-sam* nil)
(defvar *ssaa-stream* nil) ; :primitive :points

(defclass camera ()
  ((pos :initform (vec3 0 0 0) :accessor pos)
   (rot :initform (qid) :accessor rot)))
(defclass thing ()
  ((pos :initform (vec3 0 0 0) :accessor pos)
   (rot :initform (qid) :accessor rot)
   (%scale :accessor scale
           :initarg :scale
           :initform 1.0f0)
   (%stream :accessor buf-stream
            :initarg :stream)
   (%sampler :accessor sampler
             :initarg :sampler)))

(defun make-thing (stream sampler)
  (make-instance 'thing :stream stream :sampler sampler))

(defvar *camera* (make-instance 'camera))
(defvar *camera2* (make-instance 'camera))
(defvar *current-camera* *camera*)
(defvar *things*)
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
  (= (* (v4dot a a) (v4dot b b)) (expt (v4dot a b) 2)))


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
    (v! pos -1 1)))
(defun-g crosshair-frag2 ()
  (v! 1 0 0 0))

(defpipeline-g crosshair ()
  (crosshair-vert :vec2)
  (crosshair-frag2))

(defun tick ()
  (float (get-internal-real-time)))
(defun world->view (&optional (camera *camera*))
  (m4* (q->m4 (qinv (rot camera)))
       (v3->m4tr! (m4id) (v3neg (pos camera)))))
(defun model->world (thing)
         (m4* (v3->m4tr! (m4id) (pos thing))
              (m4* (q->m4 (rot thing))
                   (v3->m4scale! (m4id) (v3scale (v! 1 1 1)
                                                 (scale thing))))))

(defvar *turbo* 10)
(defvar *pitch* 0)
(defvar *yaw* 0)
(defvar *down* nil)
(defvar *active* nil)
(defvar *mpos* (vec2 0 0))
(defvar *mdelta* (vec2 0 0))
(defun update-camera (camera)
  (when *mouse-captured*
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
      (setf (rot camera) (v! 1 0 0 0)
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

(defvar *pick-fbo* nil)
(defun pick-setup (&optional (scale 1.0)
                   &aux (dim (ilist (v2scale xdim scale))))
  (setf *ssaa-fbo* (make-fbo (list 0 :dimensions dim)
                             (list :d :dimensions dim))))
(defun pick-kill ()
  (free *ssaa-fbo*))

(defun-g picking-vert ((vert g-pnt)
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
    clip-pos))
(defun-g picking-frag (&uniform
               (picking-color :vec3))
  (v! picking-color 1))
(defpipeline-g picking-prog ()
  (picking-vert g-pnt)
  (picking-frag))

(defun id-to-color (num)
  (v! (/ (ldb (byte 8 0) num) 255.0f0)
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
    (setf (clear-color) (v! 1 1 1 1))
    (clear)
    (map-g #'picking-prog nil
           :world->view (world->view *current-camera*)
           :view->clip (m4persp
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
  (let* ((pos (v! vert (/ 1.0f0 length)))
         (pos (* model->world pos))
         (pos (* world->view pos))
         (pos (* view->clip pos)))
    pos))
(defun-g line-frag ()
  (v! 1 0 0 0))

(defpipeline-g line-prog (:lines)
  (line-vert :vec3)
  (line-frag))

(defvar *under-crosshair* nil)
(defvar *mouse-captured* nil)

(defun step-demo ()
  (declare (optimize (debug 3)))
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
      (setf (pos obj) (v3cp (pos *current-camera*)))
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
  (update-camera *current-camera*)
  (when (and *down* (not *active*))
    (format t "~a~%" *fps*)
    #+(or)(let ((obj (make-thing *stream* *sam*)))
      (setf (pos obj) (v3+ (m4*v3 (q->m4 (rot *current-camera*))
                                  (v! 0 0 -5))
                           (pos *current-camera*)))
      (push obj *things*)))

  (setf (resolution (current-viewport))
        (surface-resolution (current-surface (cepl-context))))
  (clear)
  (map-g #'pipeline nil
         :sam *sam*
         :light-pos *light-pos*
         :cam-pos (pos *current-camera*)
         :world->view (world->view *current-camera*)
         :view->clip ;(m4ortho -40.0f0 40.0f0 -40.0f0 40.0f0 0.1f0 300.0f0)
         ;#+(or)
         (m4persp
                      (radians 60f0)
                      (/ (.x (viewport-resolution (current-viewport)))
                         (.y (viewport-resolution (current-viewport))))
                      0.2
                      300f0))
  (clear-fbo *ssaa-fbo*)
  (with-fbo-bound (*ssaa-fbo* :with-blending nil)
    (loop :for thing :in *things* :do
      (update-thing thing)
      (map-g #'pipeline (buf-stream thing)
             :sam (sampler thing)
             :model->world (model->world thing)))
    (map-g #'line-prog *line*
           :model->world (m4invt (world->view *camera*))
           :world->view (world->view *current-camera*)
           :view->clip (m4persp
                        (radians 60f0)
                        (/ (.x (viewport-resolution (current-viewport)))
                           (.y (viewport-resolution (current-viewport))))
                        0.2
                        300f0)
           :length 200.0f0))
  (map-g #'ssaa-pipe *ssaa-stream*
         :scene *ssaa-sam*)
  (map-g #'crosshair *tri-stream*
         :ratio (viewport-resolution (current-viewport))
         :size 100.0f0)
  (nineveh:draw-tex-br *sam*)
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
    (setf (.y pos) (mod (- (.y pos) (* *delta* 0))
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
  (when *index*
    (free *index*))
  (when *rot*
    (free *rot*))
  (when *pos*
    (free *pos*))
  (bind (((vert index)
          (nineveh.mesh.data.primitives:cube-gpu-arrays)))
    (setf *array* vert
          *index* index
          *stream* (make-buffer-stream (list vert)
                                       :index-array index))))
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
  (setf *tri* (make-gpu-array (list (v! 0.0f0  0.0f0)
                                    (v! 0.0f0 -0.5f0)
                                    (v! 0.5f0  0.0f0))
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
