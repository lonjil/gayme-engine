(in-package #:lafps)

(defclass player (object)
  ((%camera :accessor camera :initarg :camera :type (or camera null))
   (%yaw :accessor yaw :initarg :yaw :type real)
   (%pitch :accessor pitch :initarg :pitch :type real))
  (:default-initargs
   :camera (make-instance 'camera :pos (v3:vec 0.0f0 1.0f0 0.0f0))
   :yaw 0.0f0
   :pitch 0.0f0))

(defmethod update :after ((player player) &optional (*dt* *dt*))
  (with-accessors* (camera pos rot pitch momentum)
      player
    (v3:+! pos
           pos
           (v3:scale momentum *dt*))
    (setf (v3:y pos) 0.0f0 #+(or)(get-floor-height pos))
    (setf (rot camera) rot
          (pos camera) (v3:+ pos (v3:vec 0.0f0 1f0 0f0)))))
(defgeneric apply-input (thing)
  (:documentation "Update thing according to current inputs."))
(defun clamp (min max val)
  (max min (min (min max val))))
(defmethod apply-input ((player player))
  (with-accessors* (yaw pitch momentum rot)
      player
    (when *mouse-captured*
      (setf yaw (mod (+ yaw (* -0.01 (v2:x *mdelta*)))
                     (* 2 +fpi+))
            pitch (clamp (/ +fpi+ -2)
                         (/ +fpi+ 2)
                         (+ pitch (* -0.01 (v2:y *mdelta*)))))
      (q:*! rot
            (q:rotate q:+id+ (v3:vec 0 yaw 0))
            (q:rotate q:+id+ (v3:vec pitch 0 0))))
    (let* ((mult (* *delta* (coerce *turbo* 'single-float) 50))
           (dir (v3:negate (q2ray rot)))
           (forward (v3:negate (q2ray (q:rotate q:+id+ (v3:vec 0 yaw 0)))))
           (right (q2ray (q:rotate q:+id+ (v3:vec 0
                                                   (+ yaw (radians 90))
                                                   0))))
           (up (v3:vec 0 1 0))
           (final (v3:vec 0 0 0))
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
      (setf momentum (v3:scale (v3:normalize final) mult))
      (when *active* (format t "~a~%" yaw))
                                        ;(v3+! (pos camera) (pos camera) (v3scale (v3normalize final) mult))
      (when (s:keyboard-button keyb k:key.r)
        (setf rot (q:quat 1 0 0 0)
              yaw 0f0
              pitch 0f0)))))
