(uiop/package:define-package :clcv/io/image
  (:use :common-lisp)
  (:export #:imread #:imwrite))

(in-package :clcv/io/image)

;; Returns an image object (list of 2d arrays)
(defun imread (path)
  (let* ((image (opticl:read-image-file path))
         (dims (array-dimensions image))
         (rows (first dims))
         (cols (second dims))
         (channels (third dims)))
    (loop for k from 0 below channels
       collect (let ((mat (make-array (list rows cols))))
                 (dotimes (i rows)
                   (dotimes (j cols)
                     (setf (aref mat i j) (aref image i j k))))
                 mat))))


(defun imwrite (file mats)
  (let ((rows (mats-height mats))
        (cols (mats-width mats))
        (channels (length mats))
        (mat1 (first mats))
        (mat2 (second mats))
        (mat3 (third mats)))
    (let ((image (make-array (list rows cols channels))))
      (dotimes (k channels)
        (dotimes (i rows)
          (dotimes (j cols)
            (setf (aref image i j k) (aref mat i j)))))
      (opticl:write-image-file path image))))
