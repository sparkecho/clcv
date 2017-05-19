;;;; clcv.lisp
(uiop/package:define-package :clcv/clcv
    (:nicknames :clcv)
  (:use :common-lisp)
  (:export #:clcv)
  (:use-reexport :clcv/core/all
                 :clcv/matrix/all
                 :clcv/dip/all
                 :clcv/gui/all
                 :clcv/io/all
                 :clcv/ml/all
                 :clcv/graphics/all))


(in-package #:clcv)


(defun clcv ()
  (format t "CLCV~%"))


(provide "clcv")
(provide "CLCV")
