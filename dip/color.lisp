(uiop/package:define-package :clcv/dip/color
    (:use :common-lisp
          :clcv/core/all)
  (:import-from :opticl-core
                #:with-image-bounds
                #:pixel
                #:do-pixels)
  (:export #:convert-color))

(in-package :clcv/dip/color)


(defgeneric convert-color (image flag)
  (:documentation "Convert image from a color space to another color space."))


;;; Convert a RGB image to a grayscale image.
(defmethod convert-color (image (flag (eql :rgb2gray)))
  (with-image-bounds (height width)
      image
    (let ((gray-image (make-image height width '8uc1)))
      (declare (type 8uc1 gray-image)
               (type 8uc3 image))
      (do-pixels (i j)
          image
        (multiple-value-bind (r g b)
            (pixel image i j)
          (setf (pixel gray-image i j)
                (round (+ (* r 0.2989)
                          (* g 0.5870)
                          (* b 0.1140))))))
      gray-image)))


;;; Convert a grayscale image to rgb image.
(defmethod convert-color (image (flag (eql :gray2rgb)))
  (with-image-bounds (height width)
      image
    (let ((rgb-image (make-image height width '8uc3)))
      (declare (type 8uc1 image)
               (type 8uc3 rgb-image))
      (do-pixels (i j)
          image
        (let ((grayscale (pixel image i j)))
          (setf (pixel rgb-image i j)
                (values grayscale grayscale grayscale))))
      rgb-image)))


(defmethod convert-color (image (flag (eql :rgb2ycbcr)))
  (with-image-bounds (height width)
      image
    (let ((ycbcr-image (make-image height width '8uc3))
          (delta 128))
      (declare (type 8uc3 ycbcr-image)
               (type 8uc3 image))
      (flet ((to-ycbcr (r g b delta)
               (let ((y (round (+ (* r 0.299)
                                  (* g 0.587)
                                  (* b 0.114)))))
                 (values y
                         (round (+ (* (- b y) 0.564) delta))
                         (round (+ (* (- r y) 0.713) delta))))))
        (do-pixels (i j)
            image
          (multiple-value-bind (r g b)
              (pixel image i j)
            (setf (pixel ycbcr-image i j)
                  (to-ycbcr r g b delta)))))
      ycbcr-image)))


(defmethod convert-color (image (flag (eql :ycbcr2rgb)))
  (with-image-bounds (height width)
      image
    (let ((rgb-image (make-image height width '8uc3))
          (delta 128))
      (declare (type 8uc3 image)
               (type 8uc3 rgb-image))
      (labels ((normalize (val)
                 (if (> val 255)
                     255
                     val))
               (to-rgb (y cb cr delta)
                 (let ((cb-delta (- cb delta))
                       (cr-delta (- cr delta)))
                   (let ((r (round (+ y (* 1.403 cr-delta))))
                         (g (round (- y (* 0.714 cr-delta) (* 0.344 cb-delta))))
                         (b (round (+ y (* 1.773 cb-delta)))))
                     (values (normalize r)
                             (normalize g)
                             (normalize b))))))
        (do-pixels (i j)
            image
          (multiple-value-bind (y cb cr)
              (pixel image i j)
            (setf (pixel rgb-image i j)
                  (to-rgb y cb cr delta)))))
      rgb-image)))
