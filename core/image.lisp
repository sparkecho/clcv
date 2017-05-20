(uiop/package:define-package :clcv/core/image
    (:use :common-lisp)
  (:export *image-types*
           #:make-image))

(in-package :clcv/core/image)


(defparameter *image-types* (make-hash-table))

(setf (gethash :8uc1 *image-types*) '(1 . (unsigned-byte 8))
      (gethash :8uc2 *image-types*) '(2 . (unsigned-byte 8))
      (gethash :8uc3 *image-types*) '(3 . (unsigned-byte 8))
      (gethash :8uc4 *image-types*) '(4 . (unsigned-byte 8))

      (gethash :8sc1 *image-types*) '(1 . (signed-byte 8))
      (gethash :8sc2 *image-types*) '(2 . (signed-byte 8))
      (gethash :8sc3 *image-types*) '(3 . (signed-byte 8))
      (gethash :8sc4 *image-types*) '(4 . (signed-byte 8))

      (gethash :16uc1 *image-types*) '(1 . (unsigned-byte 16))
      (gethash :16uc2 *image-types*) '(2 . (unsigned-byte 16))
      (gethash :16uc3 *image-types*) '(3 . (unsigned-byte 16))
      (gethash :16uc4 *image-types*) '(4 . (unsigned-byte 16))

      (gethash :16sc1 *image-types*) '(1 . (signed-byte 16))
      (gethash :16sc2 *image-types*) '(2 . (signed-byte 16))
      (gethash :16sc3 *image-types*) '(3 . (signed-byte 16))
      (gethash :16sc4 *image-types*) '(4 . (signed-byte 16))

      (gethash :32uc1 *image-types*) '(1 . (unsigned-byte 32))
      (gethash :32uc2 *image-types*) '(2 . (unsigned-byte 32))
      (gethash :32uc3 *image-types*) '(3 . (unsigned-byte 32))
      (gethash :32uc4 *image-types*) '(4 . (unsigned-byte 32))

      (gethash :sfc1 *image-types*) '(1 . single-float)
      (gethash :sfc2 *image-types*) '(2 . single-float)
      (gethash :sfc3 *image-types*) '(3 . single-float)
      (gethash :sfc4 *image-types*) '(4 . single-float)

      (gethash :dfc1 *image-types*) '(1 . double-float)
      (gethash :dfc2 *image-types*) '(2 . double-float)
      (gethash :dfc3 *image-types*) '(3 . double-float)
      (gethash :dfc4 *image-types*) '(4 . double-float))


(defun make-image (height width type)
  (let ((spec (gethash type *image-types*)))
    (make-image% height width (car spec) (cdr spec))))


(defun make-image% (height width channels element-type)
  (if (= channels 1)
      (make-array (list height width)
                  :element-type element-type)
      (make-array (list height width channels)
                  :element-type element-type)))
