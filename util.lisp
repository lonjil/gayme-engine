(in-package :lafps)

;; non-consing
(defun vec3->vec4! (out vec3 &key (w 1.0))
  (v3:with-components ((|3| vec3))
    (v4:with-components ((|4| out))
      (psetf 4x 3x
             4y 3y
             4z 3z
             4w w)))
  out)

;; consing
(defun vec3->vec4 (vec3 &key (w 1.0))
  (vec3->vec4! (v4:vec) vec3 :w w))

;; non-consing
(defun vec2->vec4! (out vec2 &key (z 0.0) (w 1.0))
  (v2:with-components ((|2| vec2))
    (v4:with-components ((|4| out))
      (psetf 4x 2x
             4y 2y
             4z z
             4w w)))
  out)

;; consing
(defun vec2->vec4 (vec2 &key (z 0.0) (w 1.0))
  (vec2->vec4! (v4:vec) vec2 :z z :w w))

(defun radians (degrees)
  (declare (type single-float degrees)
           (optimize (speed 3) (debug 0) (safety 0)))
  (* degrees #.(coerce pi 'single-float) #.(/ 1.0f0 180.0f0)))
(cffi:defcstruct timespec
  (seconds :long)
  (nanoseconds :long))
(cffi:defcfun "clock_gettime" :int
  (clock-type :int)
  (timespec (:pointer (:struct timespec))))
(defun get-ns-time ()
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (cffi:with-foreign-object (ts '(:pointer (:struct timespec)))
    (clock-gettime 1 ts)
    (cffi:with-foreign-slots ((seconds nanoseconds) ts (:struct timespec))
      (the fixnum (+ (the fixnum (* 1000000000 (the fixnum seconds)))
                     (the fixnum nanoseconds))))))

(defmacro with-accessors* (accessors instance &body body)
  `(with-accessors ,(loop :for x :in accessors :collect (list x x))
       ,instance
     ,@body))
(defun rotate-vec3! (out vec quat)
  (let* ((r (q:from-vec3 vec))
         (q-conj (q:conjugate quat)))
    (q:to-vec3! out (q:* (q:* quat r) q-conj))))
(defun rotate-vec3 (vec quat)
  (rotate-vec3! (v3:vec) vec quat))
(defun q2ray! (out-vec quat)
  (rotate-vec3! out-vec (v3:vec 0 0 1) quat))
(defun q2ray (quat)
  (q2ray! (v3:vec) quat))

(defun v4parallelp (a b)
  (= (* (v4:dot a a) (v4:dot b b)) (expt (v4:dot a b) 2)))

(defun clamp (min max val)
  (max min (min (min max val))))
