
;;;; game.lispa

(in-package #:game)

(defun comment-sexp (stream char args)
  (declare (ignore char args))
  (let ((*read-eval* nil))
    (ignore-errors (read stream)))
  (values))

(cffi:defcstruct timespec
  (seconds :long)
  (nanoseconds :long))
(cffi:defcfun "clock_gettime" :int
  (clock-type :int)
  (timespec (:pointer (:struct timespec))))
(defun get-ns-time ()
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (with-foreign-object (ts '(:pointer (:struct timespec)))
    (clock-gettime 1 ts)
    (with-foreign-slots ((seconds nanoseconds) ts (:struct timespec))
      (declare (type fixnum seconds nanoseconds))
      (the fixnum (+ (the fixnum (* 1000000000 seconds)) nanoseconds)))))

;;; "game" goes here. Hacks and glory await!

(defvar *array* nil) ; vertices of model
(defvar *stream* nil) ; stream for drawing the models
(defvar *running* nil) ; are we running?
(defvar *index* nil) ; maps vertices to triangles
(defvar *tex* nil) ; model texture (dirt:load-blah "test.jpg")
(defvar *sam* nil) ; (sample *tex*)
(defvar *model->world* nil) ; gpu-array of model->world matrices (mat4)

(defvar *point* nil)
(defvar *mouse-captured* nil)

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
(defvar *things* (loop repeat 4000
                       collect (make-instance 'thing)))
(defvar *light-pos* (vec3 0 30 -5))

(defvar *delta* 1)
(defvar *fps* 0)
(defvar *fps-wip* 0)
(defvar *stepper* (make-stepper (seconds 1)))


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

(defun-g vertex ((id :uint))
  (let* ((inv (if (< id 7)
                  (ivec3 0 0 0)
                  (ivec3 1 0 1)))
         (id (if (< id 7)
                 id
                 (- 13 id)))
         (id1 (>> id 1))
         (id2 (>> id 2))
         (x (- (* (float
                   (bit-xor (bit-and 1 (bit-ior id id2)) (.x inv)))
                  2)
               1))
         (y (- (* (float
                   (bit-xor (bit-and 1 (bit-xor (bit-xor id1 id2)
                                                (bit-and id id2))) (.y inv)))
                  2)
               1))
         (z (- (* (float
                   (bit-xor (bit-and 1 (bit-and (bit-xor id #xFFFFFFFF)
                                                id2)) (.z inv)))
                  2)
               1)))
    (v! x y z)))

(defun-g testv (&uniform
                (world->view :mat4)
                (view->clip :mat4))
  (let* ((i gl-instance-id)
         (mask #b111111)
         (x (float (* 8 (bit-and i mask))))
         (y (float (* 8 (bit-and (>> i 6) mask))))
         (z (float (* -8 (bit-and (>> i 12) mask)))))
    (values (* view->clip
               world->view
               (v! (+ (v! x y z) (vertex gl-vertex-id)) 1)))))

(defun-g testf ((uv :vec2) &uniform (sam :sampler-2d))
  (texture sam uv))
(defun-g testf (&uniform (sam :sampler-2d))
  (v! 1 0 0 0))

(defpipeline-g prog-test (:triangle-strip)
  :vertex (testv)
  :fragment (testf))

(defun hack (len)
  (let ((foo (make-buffer-stream nil :primitive :triangle-strip)))
    (setf (buffer-stream-length foo) len)
    foo))

(defun step-demo ()
  (declare (optimize (debug 3)))
  (update-repl-link)

  (incf *fps-wip*)
  (when (funcall *stepper*)
    (setf *fps* *fps-wip*
          *fps-wip* 0))
  (setf *delta* (/ 1.0 *fps*))


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
  (update-camera *camera*)
  (when (and *down* (not *active*))
    (format t "~a~%" *fps*))
  (setf (resolution (current-viewport))
        (surface-resolution (current-surface (cepl-context))))
  (clear)
  (clear-fbo *ssaa-fbo*)
  (with-fbo-bound (*ssaa-fbo* :with-blending nil)
    (with-instances (* 64 64 64)
      (map-g #'prog-test *stream*
             :sam *sam*
             :world->view (world->view *camera*)
             :view->clip (m4persp
                          (radians 75f0)
                          (/ (.x (viewport-resolution (current-viewport)))
                             (.y (viewport-resolution (current-viewport))))
                          0.2
                          60000f0))))
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
                     (- (random 500) 250)
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
                        500f0))))

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
  (when *point*
    (loop :for x :in (buffer-stream-gpu-arrays *point*)
          :do (free x))
    (free *point*))
  (bind (((vert index)
          (nineveh.mesh.data.primitives:cube-gpu-arrays)))
    (setf *stream* (make-buffer-stream (make-gpu-array (list (v! 0 0 0))
                                                      :element-type :vec3)
                                      :primitive :points))))
(defun awoo ()
  (when *sam* (free *sam*))
  (when *tex* (free *tex*))
  (setf *tex* (dirt:load-image-to-texture "./awoo.jpg")
        *sam* (sample *tex*)))
