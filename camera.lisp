(in-package #:lafps)

(defclass camera (entity)
  ((%fovy :accessor fovy :initarg :fovy :type real)
   (%zoom :accessor zoom :initarg :zoom :type real)
   (%mode :accessor mode :initarg :mode
          :type (member :perspective :orthographic)))
  (:default-initargs
   :fovy (radians 50)
   :zoom 1
   :mode :perspective)
  (:documentation "\"This is not documentation\""))

(defun camera-transform (camera)
  (m4:* (m4:set-projection/perspective
         (fovy camera)
         (/ (c:viewport-resolution-x (c:current-viewport))
            (c:viewport-resolution-y (c:current-viewport)))
         0.01
         100.0)
        (m4:* (q:to-mat4 (q:inverse (rot camera)))
              (m4:set-translation m4:+id+ (v3:negate (pos camera))))))
(defun foop (camera)
  (m4:* (q:to-mat4 (q:inverse (rot camera)))
        (m4:set-translation m4:+id+ (v3:negate (pos camera)))))

(defmethod apply-input ((camera camera))
  (let* ((kb (s:keyboard))
         (rot (rot camera))

         (forward (rotate-vec3 (v3:vec 0.0 0.0 -1.0) rot))
         (right (rotate-vec3 (v3:vec 1.0 0.0 0.0) rot))
         (up (v3:vec 0.0 1.0 0.0))
         (xz-forward (v3:normalize (v3:vec (v3:x forward)
                                            0.0f0
                                            (v3:z forward))))
         (final (v3:vec)))
    (when (s:keyboard-button kb k:key.w)
      (v3:+! final final forward))
    (when (s:keyboard-button kb k:key.s)
      (v3:-! final final forward))
    (when (s:keyboard-button kb k:key.d)
      (v3:+! final final right))
    (when (s:keyboard-button kb k:key.a)
      (v3:-! final final right))
    (when (s:keyboard-button kb k:key.space)
      (v3:+! final final up))
    (when (s:keyboard-button kb k:key.lctrl)
      (v3:-! final final up))
    (when (s:keyboard-button kb k:key.e)
      (v3:+! final final xz-forward))
    (when (s:keyboard-button kb k:key.f)
      (v3:-! final final xz-forward))
    (v3:+! (pos camera) (pos camera) final)
    (when *mouse-captured*
      (setf *yaw* (mod (+ *yaw* (* -0.01 (v2:x *mdelta*)))
                       (* 2 +fpi+))
            *pitch* (clamp (/ +fpi+ -2)
                           (/ +fpi+ 2)
                           (+ *pitch* (* -0.01 (v2:y *mdelta*)))))
      (q:*! rot (q:rotate-euler q:+id+ (v3:vec 0 *yaw* 0))
            (q:rotate-euler q:+id+ (v3:vec *pitch* 0 0))))
    (when (s:keyboard-button kb k:key.r)
      (setf rot q:+id+
            *yaw* 0.0f0 *pitch* 0.0f0
            (pos camera) (v3:vec 0 0 0)))))
