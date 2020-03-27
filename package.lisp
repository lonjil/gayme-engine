;;;; package.lisp
(in-package :cl-user)

(defpackage #:lafps
  (:use #:cl)
  (:local-nicknames
   (#:c #:cepl)
   (#:m4 #:origin.mat4)
   (#:m3 #:origin.mat3)
   (#:v2 #:origin.vec2)
   (#:v3 #:origin.vec3)
   (#:v4 #:origin.vec4)
   (#:q #:origin.quat)
   (#:v #:vari)
   (#:s #:skitter)
   (#:k #:skitter.sdl2.keys)
   (#:t #:temporal-functions)))
