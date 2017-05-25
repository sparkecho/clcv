(uiop/package:define-package :clcv/dip/geometry
    (:use :common-lisp
          :clcv/core/all)
  (:import-from :opticl-core
                #:with-image-bounds
                #:pixel
                #:do-pixels
                #:set-region-pixels)
  (:export #:crop
           #:copyf))

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


;;; Repeat `value' `number' times, to from a multiple value
(defun repeat (value number)
  (values-list (loop for i fixnum from 0 below number collect value)))

(defun copyf (src-image dst-image &key (y 0) (x 0) point)
  (flet ((copy-to% (src-image dst-image y x)
           (with-image-bounds (src-h src-w src-c) src-image
             (with-image-bounds (dst-h dst-w dst-c) dst-image
               (cond ((= src-c dst-c)
                      (set-region-pixels (i j y x
                                            (min (+ src-h y) dst-h)
                                            (min (+ src-w x) dst-w))
                          dst-image
                        (pixel src-image (- i y) (- j x))))
                     (t (cond ((= src-c 1)
                               (set-region-pixels (i j y x
                                                     (min (+ src-h y) dst-h)
                                                     (min (+ src-w x) dst-w))
                                   dst-image
                                 (repeat (pixel src-image (- i y) (- j x)) dst-c)))
                              (t (dotimes (k (min src-c dst-c))
                                   (loop for i fixnum
                                      from y below (min (+ src-h y) dst-h)
                                      do (loop for j fixnum
                                            from x below (min (+ src-w x) dst-w)
                                            do (setf (aref dst-image i j k)
                                                     (aref src-image (- i y) (- j x) k)))))))))))))
    (cond ((null point) (copy-to% src-image dst-image y x))
          (t (copy-to% src-image dst-image (point-x point) (point-y point))))))
