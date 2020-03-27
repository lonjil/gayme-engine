(in-package #:lafps)
(defvar *dt* 0.1f0)
(defvar *delta* 1)
(defvar *fps* 0)
(defvar *fps-wip* 0)
(defvar *stepper* (t:make-stepper (t:seconds 1)))
(defun now ()
  (/ (float (get-internal-real-time))
     1000))
