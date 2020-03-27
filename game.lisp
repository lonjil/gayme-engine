
;;;; game.lispa

(in-package #:lafps)


;;; "game" goes here. Hacks and glory await!

(defvar *running* nil) ; are we running?






(defun step-demo ()
  (declare (optimize (debug 3)))
  (livesupport:update-repl-link)
  (render)
  (do-input)
  (apply-input *camera*))

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

