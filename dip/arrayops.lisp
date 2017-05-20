(uiop/package:define-package :clcv/dip/arrayops
    (:use :common-lisp)
  (:import-from :opticl-core
                #:with-image-bounds
                #:copy-array)
  (:export #:split-image
           #:extract-channel
           #:merge-channels
           #:merge-channel-list))

(in-package :clcv/dip/arrayops)


;;; Split each channel of an image into an 2d array
(defun split-image (image)
  (with-image-bounds (height width channels)
      image
    (declare (ignore height width))
    (if channels
        (loop for i from 0 below channels
           collect (extract-channel image i))
        (list (copy-array image)))))


;;; Extract a specific channel from image
(defun extract-channel (image channel-index)
  (with-image-bounds (height width)
      image
    (let ((channel (make-array (list height width)
                               :element-type (array-element-type image))))
      (dotimes (i height)
        (dotimes (j width)
          (setf (aref channel i j) (aref image i j channel-index))))
      channel)))


;;; Merge several channels into an image
(defun merge-channels (channel &rest channels)
  (merge-channel-list (cons channel channels)))


(defun merge-channel-list (channels)
  (let ((num (length channels)))
    (if (= num 1)
        (copy-array channels)
        (with-image-bounds (height width)
            (car channels)
          (let ((image (make-array (list height width num)
                                   :element-type (array-element-type (car channels)))))
            (loop for e in channels
               for k = 0 then (+ k 1)
                 do (dotimes (i height)
                      (dotimes (j width)
                        (setf (aref image i j k) (aref e i j)))))
            image)))))
