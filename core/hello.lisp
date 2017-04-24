(uiop/package:define-package :clcv/core/hello
  (:use :common-lisp)
  (:export #:hello))

(in-package :clcv/core/hello)


(defun hello ()
  "Hello from clcv/core/hello")
