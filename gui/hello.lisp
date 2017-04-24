(uiop/package:define-package :clcv/gui/hello
  (:use :common-lisp)
  (:export #:hello))

(in-package :clcv/gui/hello)


(defun hello ()
  "Hello from clcv/gui/hello")
