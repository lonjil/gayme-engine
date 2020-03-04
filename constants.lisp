(in-package #:lafps)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (boundp '+fpi+)
    (defconstant +fpi+ #.(coerce pi 'single-float))))
