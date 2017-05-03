(uiop/package:define-package :clcv/io/image
  (:use :common-lisp :clcv-core)
  (:export #:imread #:imwrite))

(in-package :clcv/io/image)


(defun imread (file &optional option)
  (declare (ignore option))
  (opticl:read-image-file file))
