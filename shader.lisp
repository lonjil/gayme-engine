(in-package #:lafps)


(c:defun-g vert-plain-tex ((vert c:g-pnt) &uniform
                           (now :float) (persp :mat4)
                           (cam :mat4) (obj :mat4))
  (let* ((pos (* persp cam obj (vari:vec4 (* 4 (c:pos vert)) 1))))
    (values pos (c:tex vert))))
(c:defun-g frag-plain-tex ((uv :vec2) &uniform (sam :sampler-2d))
  (vari:texture sam uv))
(c:defpipeline-g plain ()
  (vert-plain-tex c:g-pnt)
  (frag-plain-tex :vec2))

(c:defun-g vert-plain ((vert c:g-pnt) &uniform
                       (persp :mat4) (cam :mat4) (obj :mat4))
  (* persp cam obj (vari:vec4 (* 4 (c:pos vert)) 1)))
(c:defun-g frag-lamp (&uniform (light :vec3))
  (vari:vec4 1 0.2 1 ))
(c:defun-g frag-color (&uniform (light :vec3) (color :vec3))
  (* light color))
(c:defun-g frag-light-tex ((uv :vec2) &uniform (sam :sampler-2d)
                           (light :vec3))
  (let* ((light (v:vec4 light 1))
         (pixel (v:texture sam uv))
         (ambient (* light 0.2)))
    (* ambient pixel)))

(c:defpipeline-g lamp ()
  (vert-plain c:g-pnt)
  (frag-lamp))

(c:defpipeline-g vert-lit ()
  (vert-plain-tex c:g-pnt)
  (frag-light-tex :vec2))
