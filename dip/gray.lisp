(uiop/package:define-package :clcv/dip/gray
  (:use :common-lisp)
  (:export #:rgb-to-gray))

(in-package :clcv/dip/gray)


(defun rgb-to-gray (image)
  (opticl:coerce-image image 'opticl:8-bit-gray-image))

;; (defun rgb-to-gray (mats)
;;   (let* ((mat1 (first mats))
;;          (mat2 (second mats))
;;          (mat3 (third mats))
;;          (rows (array-dimension mat1 0))
;;          (cols (array-dimension mat1 1))
;;          (gray (make-array (list rows cols))))
;;     (dotimes (i rows)
;;       (dotimes (j cols)
;;         (setf (aref gray i j)
;;               (truncate (+ (aref mat1 i j)
;;                            (aref mat2 i j)
;;                            (aref mat3 i j))
;;                         3))))
;;     (cons gray (cons gray (cons gray nil)))))
