;;;; package.lisp
(in-package :defpackage+-user-1)

(defpackage+ #:game
  (:use #:cl
        #:cepl
        #:livesupport
        #:bind
        #:nineveh
        #:temporal-functions
        #:box.math.vari
        #:skitter
        #:cepl.skitter)
  (:local-nicknames (#:v2 #:box.math.vec2)
                    (#:v3 #:box.math.vec3)
                    (#:v4 #:box.math.vec4)
                    (#:q #:box.math.quat)
                    (#:m2 #:box.math.mat2)
                    (#:m3 #:box.math.mat3)
                    (#:m4 #:box.math.mat4)))
