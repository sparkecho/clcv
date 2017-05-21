(uiop/package:define-package :clcv/dip/all
    (:nicknames :clcv-dip)
  (:use :common-lisp)
  (:use-reexport :clcv/dip/arrayops
                 :clcv/dip/geometry
                 :clcv/dip/color))


(provide "clcv-dip")
(provide "CLCV-DIP")
