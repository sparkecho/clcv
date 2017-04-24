(uiop/package:define-package :clcv/dip/hello
  (:use :common-lisp)
  (:export #:hello))

(in-package :clcv/dip/hello)


(defun hello ()
  "Hello from clcv/dip/hello")
