(uiop/package:define-package :clcv/io/hello
  (:use :common-lisp)
  (:export #:hello))

(in-package :clcv/io/hello)


(defun hello ()
  "Hello from clcv/io/hello")
