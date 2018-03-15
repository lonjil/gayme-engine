;;;; game.asd

(asdf:defsystem #:game
  :description "Describe game here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:livesupport
               #:bordeaux-threads
               #:metabang-bind
               #:cepl
               #:varjo
               #:cepl.sdl2
               #:gamebox-math.vari
               #:dirt
               #:nineveh
               #:temporal-functions
               #:cepl.skitter.sdl2
               #:defpackage-plus)
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "game")))
