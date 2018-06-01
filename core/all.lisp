(uiop/package:define-package :clcv/core/all
    (:nicknames :clcv-core)
  (:use :common-lisp)
  (:use-reexport :clcv/core/type
                 :clcv/core/image
                 :clcv/core/array
                 :clcv/core/point))


(provide "clcv-core")
(provide "CLCV-CORE")
