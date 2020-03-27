;;;; game.asd

(asdf:defsystem #:lafps
  :description "Describe game here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:livesupport
               #:varjo
               #:cepl.sdl2
               #:origin
               #:dirt
               #:metabang-bind
               #:nineveh
               #:temporal-functions
               #:cepl.skitter.sdl2)
  :serial t
  :components ((:file "package")
               (:file "constants")
               (:file "globals")
               (:file "util")
               (:file "input")
               (:file "space")
               (:file "time")
               (:file "physics")
               (:file "camera")
               (:file "shader")
               (:file "rendering")
               (:file "player")
               (:file "misc")
               (:file "game")
               ))
