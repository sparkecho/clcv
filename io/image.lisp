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

;;; Read the image to a two dimensions array
(defun imread2 (path)
  (let* ((image (opticl:read-image-file path))
         (rows (array-dimension image 0))
         (cols (array-dimension image 1))
         (channels (array-dimension image 2))
         (matrix (make-array (list rows (* cols channels)))))
    (dotimes (i rows)
      (dotimes (j cols)
        (dotimes (k channels)
          (setf (aref matrix i (+ (* j channels) k))
                (aref image i j k)))))
    matrix))
              


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


;;; Write a two dimensions array to an image.
(defun imwrite2 (file matrix)
  (let* ((rows (array-dimension matrix 0))
         (cols (array-dimension matrix 1))
         (image (make-array (list rows (/ cols 3) 3) :element-type '(unsigned-byte 8))))
    (dotimes (i rows)
      (dotimes (j cols)
        (multiple-value-bind (d2 d3)
            (truncate j 3)
          (setf (aref image i d2 d3) (aref matrix i j)))))
    (opticl:write-image-file file image)))
