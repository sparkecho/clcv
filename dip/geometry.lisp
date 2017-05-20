(uiop/package:define-package :clcv/dip/geometry
    (:use :common-lisp)
  (:import-from :opticl-core
                #:with-image-bounds
                #:pixel)
  (:export #:crop))

(in-package :clcv/dip/geometry)

(defun crop (image y1 x1 y2 x2)
  (with-image-bounds (height width channels)
      image
    (declare (ignore height width))
    (let ((new-rows (- y2 y1))
          (new-cols (- x2 x1)))
      (let ((new-image (make-array
                        (cons new-rows (cons new-cols (when channels (list channels))))
                        :element-type (array-element-type image))))
        (loop for i-src from y1 below y2
           for i-dest below new-rows
           do (loop for j-src from x1 below x2
                 for j-dest below new-cols
                 do (setf (pixel new-image i-dest j-dest)
                          (pixel image i-src j-src))))
        new-image))))
