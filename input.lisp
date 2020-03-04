(in-package #:lafps)

(defvar *turbo* 0)
(defvar *pitch* 0)
(defvar *yaw* 0)
(defvar *down* nil)
(defvar *active* nil)
(defvar *mpos* (v2:vec 0 0))
(defvar *mdelta* (v2:vec 0 0))
(defun update-mouse ()
  (let ((down (s:mouse-button (s:mouse) 1)))
    (setf *active* (and down *down*))
    (setf *down* down))
  (let ((new-pos (s:mouse-pos (s:mouse))))
    (v2:-! *mdelta* new-pos *mpos*)
    (setf *mpos* new-pos)))


(defvar *mouse-captured* nil)

(defun do-input ()
  (s:decay-events)
  (when (s:keyboard-button (s:keyboard) k:key.q)
    (sdl2-ffi.functions:sdl-capture-mouse 0)
    (sdl2-ffi.functions:sdl-show-cursor 1)
    (setf *mouse-captured* nil))
  (when (s:keyboard-button (s:keyboard) k:key.escape)
    (sdl2-ffi.functions:sdl-capture-mouse 1)
    (sdl2-ffi.functions:sdl-show-cursor 0)
    (setf *mouse-captured* t))
  (update-mouse))