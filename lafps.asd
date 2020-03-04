;;;; game.asd

(asdf:defsystem #:lafps
  :description "Describe game here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:livesupport
               #:varjo
               #:cepl.sdl2
               #:origin
               #:serapeum
               #:dirt
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
               (:file "physics")
               (:file "camera")
               (:file "player")
               (:file "game")
               ))
