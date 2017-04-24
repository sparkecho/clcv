(uiop/package:define-package :clcv/test/hello
  (:use :common-lisp)
  (:export #:hello))

(in-package :clcv/test/hello)


(defun hello ()
  "Hello from clcv/test/hello")
