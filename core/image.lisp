(uiop/package:define-package :clcv/core/image
    (:use :common-lisp)
  (:export #:image
           #:gray-image
           #:rgb-image
           #:rgba-image
           #:make-image
           #:image-type
           #:define-image-type
           #:1uc1
           #:2uc1
           #:4uc1
           #:4uc3
           #:4uc4
           #:8uc1
           #:8uc2
           #:8uc3
           #:8uc4
           #:8sc1
           #:8sc2
           #:8sc3
           #:8sc4
           #:16uc1
           #:16uc2
           #:16uc3
           #:16uc4
           #:16sc1
           #:16sc2
           #:16sc3
           #:16sc4
           #:32uc1
           #:32uc2
           #:32uc3
           #:32uc4
           #:32sc1
           #:32sc2
           #:32sc3
           #:32sc4
           #:fnc1
           #:fnc2
           #:fnc3
           #:fnc4
           #:sfc1
           #:sfc2
           #:sfc3
           #:sfc4
           #:dfc1
           #:dfc2
           #:dfc3
           #:dfc4))


(in-package :clcv/core/image)


(deftype image (&optional element-type channels)
  `(simple-array ,element-type
                 ,(if (numberp channels)
                      (if (= channels 1)
                          `(* *)
                          `(* * ,channels))
                      channels)))

(deftype gray-image (&optional element-type)
  `(simple-array ,element-type (* *)))

(deftype rgb-image (&optional element-type)
  `(simple-array ,element-type (* * 3)))

(deftype rgba-image (&optional element-type)
  `(simple-array ,element-type (* * 4)))


(defgeneric make-image (height width type)
  (:documentation "Generate an `height' * `width' image of type `type'"))


(defmacro define-image-type (name &optional channels (element-type t))
  "Defines a new image type. Under the covers, this results in
evaluation of the appropriate deftype and make-my-image-type
constructor functions. Returns the name of the created
type (i.e. name)."
  (let ((type (read-from-string (format nil "~A" name))))
      `(progn
         (deftype ,type ()
           `(image ,(or ,channels '*) ,(or ,element-type '*)))
         (defmethod make-image (height width (type (eql ',type)))
           (make-array (if (or (null ,channels) (= ,channels 1))
                           `(,height ,width)
                           `(,height ,width ,,channels))
                       :element-type ',element-type))
         ',type)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *image-types*
    '((1uc1 1 (unsigned-byte 1))
      (2uc1 1 (unsigned-byte 2))
      (4uc1 1 (unsigned-byte 4))
      (4uc3 3 (unsigned-byte 4))
      (4uc4 4 (unsigned-byte 4))

      (8uc1 1 (unsigned-byte 8))
      (8uc2 2 (unsbigned-byte 8))
      (8uc3 3 (unsigned-byte 8))
      (8uc4 4 (unsigned-byte 8))

      (8sc1 1 (signed-byte 8))
      (8sc2 2 (signed-byte 8))
      (8sc3 3 (signed-byte 8))
      (8sc4 4 (signed-byte 8))

      (16uc1 1 (unsigned-byte 16))
      (16uc2 2 (unsigned-byte 16))
      (16uc3 3 (unsigned-byte 16))
      (16uc4 4 (unsigned-byte 16))

      (16sc1 1 (signed-byte 16))
      (16sc2 2 (signed-byte 16))
      (16sc3 3 (signed-byte 16))
      (16sc4 4 (signed-byte 16))

      (32uc1 1 (unsigned-byte 32))
      (32uc2 2 (unsigned-byte 32))
      (32uc3 3 (unsigned-byte 32))
      (32uc4 4 (unsigned-byte 32))

      (32sc1 1 (signed-byte 32))
      (32sc2 2 (signed-byte 32))
      (32sc3 3 (signed-byte 32))
      (32sc4 4 (signed-byte 32))

      (fnc1 1 fixnum)
      (fnc2 2 fixnum)
      (fnc3 3 fixnum)
      (fnc4 4 fixnum)

      (sfc1 1 single-float)
      (sfc2 2 single-float)
      (sfc3 3 single-float)
      (sfc4 4 single-float)

      (dfc1 1 double-float)
      (dfc2 2 double-float)
      (dfc3 3 double-float)
      (dfc4 4 double-float))))


;; to define a new image type one could do:
;; (define-image-type rational-gray-image :channels 1 :element-type rational)
(macrolet
    ((frobber ()
       `(progn
          ,@(loop for image-spec in *image-types*
               collect
                 (destructuring-bind (name channels element-type)
                     image-spec
                   `(define-image-type ,name ,channels ,element-type))))))
  (frobber))


(defun image-type (image)
  (let ((element-type (array-element-type image))
        (channels (case (array-rank image)
                    (2 1)
                    (3 (array-dimension image 2))
                    (otherwise (error "Argument is not a legal image.")))))
    (let ((prefix (if (listp element-type)
                      (format nil "~A~A"
                              (second element-type)
                              (case (first element-type)
                                (unsigned-byte "U")
                                (signed-byte "S")
                                (otherwise
                                 (error "Unsupported type ~A." element-type))))
                      (case element-type
                        (fixnum "FN")
                        (single-float "SF")
                        (double-float "DF")
                        (otherwise
                         (error "Unsupported type ~A." element-type))))))
      (prog1 (read-from-string (format nil "~AC~A" prefix channels))))))
