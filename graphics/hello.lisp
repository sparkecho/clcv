(uiop/package:define-package :clcv/graphics/hello
  (:use :common-lisp)
  (:export #:hello))

(in-package :clcv/graphics/hello)


(defun hello ()
  "Hello from clcv/graphics/hello")
