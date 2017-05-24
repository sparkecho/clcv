(uiop/package:define-package :clcv/io/image
    (:use :common-lisp :clcv-core)
  (:import-from :opticl
                #:read-image-file
                #:write-image-file)
  (:export #:imread
           #:imwrite))

(in-package :clcv/io/image)


(defun imread (file &optional option)
  (declare (ignore option))
  (read-image-file file))


(defun imwrite (file image &optional option)
  (declare (ignore option))
  (write-image-file file image))
