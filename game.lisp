
;;;; game.lispa

(in-package #:lafps)


;;; "game" goes here. Hacks and glory await!

(defvar *running* nil) ; are we running?


(defvar *dt* 0.1f0)
(defun clamp (min max val)
  (max min (min (min max val))))

(defvar *things* nil)



(defun now ()
  (/ (float (get-internal-real-time))
     1000))
(defun step-demo ()
  (declare (optimize (debug 3)))
  (livesupport:update-repl-link)
  (render)
  (do-input))

(defun run-loop ()
  (when *running* (return-from run-loop))
  (setf *running* t)
  (slynk-mrepl::send-prompt (find (bt:current-thread)
                                  (slynk::channels)
                                  :key #'slynk::channel-thread))
  (unwind-protect
       (loop :while (and *running* (not (c:shutting-down-p))) :do
         (livesupport:continuable (step-demo)))
    (setf *running* nil)))
(defun stop-loop ()
  (setf *running* nil))

