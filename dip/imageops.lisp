(uiop/package:define-package :clcv/dip/imageops
    (:use :common-lisp
          :clcv/core/all)
  (:import-from :opticl-core
                #:with-image-bounds
                #:pixel
                #:pixel*
                #:do-pixels
                #:set-region-pixels)
  (:export #:imadd
           #:imsub))

(in-package :clcv/dip/imageops)


(declaim (inline bound+ bound-))


(defun bound+ (number1 number2 &optional (max-val 255))
  (min (+ number1 number2) max-val))


(defun bound- (number1 number2 &optional (min-val 0))
  (max (- number1 number2) min-val))


;; (defgeneric imadd (object1 object2)
;;   (:documentation "Add up two images or add a constant to an image."))


;; (defmethod imadd ((object1 array) (object2 array))
;; (defun imadd (object1 object2)
;;   (with-image-bounds (height1 width1 channels1)
;;       object1
;;     (with-image-bounds (height2 width2 channels2)
;;         object2
;;       (assert (and (= height1 height2) (= width1 width2)
;;                    (eql channels1 channels2))
;;               nil "Atempt to add two images with different size")
;;       (let ((image (make-image height1 width1 (image-type object1))))
;;         (do-pixels (i j)
;;             image
;;           (setf (pixel* image i j) (mapcar #'bound+
;;                                            (pixel* object1 i j)
;;                                            (pixel* object2 i j))))
;;         image))))
(defun imadd (image1 image2)
  (with-image-bounds (height1 width1 channels1)
      image1
    (with-image-bounds (height2 width2 channels2)
        image2
      (assert (and (= height1 height2) (= width1 width2)
                   (eql channels1 channels2))
              nil "Atempt to add two images with different size")
      (let ((image (make-image height1 width1 (image-type image1))))
        (do-pixels (i j)
            image
          (setf (pixel* image i j) (mapcar #'bound+
                                           (pixel* image1 i j)
                                           (pixel* image2 i j))))
        image))))


(defun imsub (image1 image2)
  (with-image-bounds (height1 width1 channels1)
      image1
    (with-image-bounds (height2 width2 channels2)
        image2
      (assert (and (= height1 height2) (= width1 width2)
                   (eql channels1 channels2))
              nil "Atempt to add two images with different size")
      (let ((image (make-image height1 width1 (image-type image1))))
        (do-pixels (i j)
            image
          (setf (pixel* image i j) (mapcar #'bound-
                                           (pixel* image1 i j)
                                           (pixel* image2 i j))))
        image))))
