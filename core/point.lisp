(uiop/package:define-package :clcv/core/point
    (:use :common-lisp)
  (:export #:point
           #:make-point
           #:point-x
           #:point-y
           #:point-z
           #:p+
           #:p-
           #:p*
           #:p/))

(in-package :clcv/core/point)


(defclass point ()
  ((x :initarg :x :accessor point-x)
   (y :initarg :y :accessor point-y)))

(defun make-point (&optional (x 0) (y 0))
  (make-instance 'point :x x :y y))
