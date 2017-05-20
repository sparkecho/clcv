(uiop/package:define-package :clcv/core/image
    (:use :common-lisp)
  (:export *image-types*
           #:make-image
           #:image-type))

(in-package :clcv/core/image)


(defparameter *image-types*
  '((:8uc1 . (1 . (unsigned-byte 8)))
    (:8uc2 . (2 . (unsigned-byte 8)))
    (:8uc3 . (3 . (unsigned-byte 8)))
    (:8uc4 . (4 . (unsigned-byte 8)))

    (:8sc1 . (1 . (signed-byte 8)))
    (:8sc2 . (2 . (signed-byte 8)))
    (:8sc3 . (3 . (signed-byte 8)))
    (:8sc4 . (4 . (signed-byte 8)))

    (:16uc1 . (1 . (unsigned-byte 16)))
    (:16uc2 . (2 . (unsigned-byte 16)))
    (:16uc3 . (3 . (unsigned-byte 16)))
    (:16uc4 . (4 . (unsigned-byte 16)))

    (:16sc1 . (1 . (signed-byte 16)))
    (:16sc2 . (2 . (signed-byte 16)))
    (:16sc3 . (3 . (signed-byte 16)))
    (:16sc4 . (4 . (signed-byte 16)))

    (:32uc1 . (1 . (unsigned-byte 32)))
    (:32uc2 . (2 . (unsigned-byte 32)))
    (:32uc3 . (3 . (unsigned-byte 32)))
    (:32uc4 . (4 . (unsigned-byte 32)))

    (:sfc1 . (1 . single-float))
    (:sfc2 . (2 . single-float))
    (:sfc3 . (3 . single-float))
    (:sfc4 . (4 . single-float))

    (:dfc1 . (1 . double-float))
    (:dfc2 . (2 . double-float))
    (:dfc3 . (3 . double-float))
    (:dfc4 . (4 . double-float))))


(defun make-image (height width type)
  (let ((spec (cdr (assoc type *image-types*))))
    (make-image% height width (car spec) (cdr spec))))


(defun make-image% (height width channels element-type)
  (if (= channels 1)
      (make-array (list height width)
                  :element-type element-type)
      (make-array (list height width channels)
                  :element-type element-type)))


(defun image-type (image)
  (if (= (array-rank image) 2)
      (rassoc (cons 1 (array-element-type image)) *image-types* :test #'equal)
      (rassoc (cons (array-dimension image 2)
                    (array-element-type image)) *image-types* :test #'equal)))
