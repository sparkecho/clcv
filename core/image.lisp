;; Basic Data Structures

(in-package :clcv)

(defstruct image-type
  (bit-depth 8 :type unsigned-byte)
  (data-type 'unsigned-byte
             :type (or (eql unsigned-byte) (eql double-float)
                       (eql float) (eql signed-byte)))
  (channels  3 :type (unsigned-byte 8)))

(defclass image ()
  ((data :initarg data
         :accessor data
         :type 'array)
   (width :initarg width
          :accessor width
          :type 'unsigned-byte)
   (height :initarg height
           :accessor height
           :type 'unsigned-byte)
   (channels :initarg channels
             :accessor channels
             :type '(unsigned-byte 8))
   (bit-depth :initarg bit-depth
              :accessor bit-depth)
   (color-type :initarg color-type
               :accessor color-type)
   (transparency :initarg transparency
                 :accessor transparency)))

;(defclass png-image (image))


;(defmethod 
