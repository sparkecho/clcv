(uiop/package:define-package :clcv/io/image
  (:use :common-lisp :clcv-core)
  (:export #:imread #:imwrite))

(in-package :clcv/io/image)

;; Returns an image object (list of 2d arrays)
(defun imread (path)
  (let* ((image (opticl:read-image-file path))
         (rows (array-dimension image 0))
         (cols (array-dimension image 1))
         (channels (array-dimension image 2)))
    (loop for k from 0 below channels
       collect (let ((mat (make-array (list rows cols))))
                 (dotimes (i rows)
                   (dotimes (j cols)
                     (setf (aref mat i j) (aref image i j k))))
                 mat))))


(defun imwrite (file mats)
  (let* ((mat1 (first mats))
         (rows (array-dimension mat1 0))
         (cols (array-dimension mat1 1))
         (channels (length mats)))
    (let ((image (make-array (list rows cols channels) :element-type :uint8)))
      (loop for mat in mats
         for k = 0 then (+ k 1)
         do (dotimes (i rows)
              (dotimes (j cols)
                (setf (aref image i j k) (aref mat i j)))))
      (opticl:write-image-file file image))))
