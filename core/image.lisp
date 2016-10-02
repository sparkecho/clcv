;; Basic Data Structures

(in-package :clcv)

(defstruct image-type
  (bit-depth 8 :type unsigned-byte)
  (data-type 'unsigned-byte
             :type (or (eql unsigned-byte) (eql double-float)
                       (eql float) (eql signed-byte)))
  (channels  3 :type (unsigned-byte 8)))

;; Unsigned 8bits
(defconstant +8uc1+ (make-image-type :bit-depth 8 :data-type 'unsigned-byte :channels 1))
(defconstant +8uc2+ (make-image-type :bit-depth 8 :data-type 'unsigned-byte :channels 2))
(defconstant +8uc3+ (make-image-type :bit-depth 8 :data-type 'unsigned-byte :channels 3))
(defconstant +8uc4+ (make-image-type :bit-depth 8 :data-type 'unsigned-byte :channels 4))
;; Signed 8bits
(defconstant +8sc1+ (make-image-type :bit-depth 8 :data-type 'signed-byte :channels 1))
(defconstant +8sc2+ (make-image-type :bit-depth 8 :data-type 'signed-byte :channels 2))
(defconstant +8sc3+ (make-image-type :bit-depth 8 :data-type 'signed-byte :channels 3))
(defconstant +8sc4+ (make-image-type :bit-depth 8 :data-type 'signed-byte :channels 4))
;; Unsigned 16bits
(defconstant +16uc1+ (make-image-type :bit-depth 16 :data-type 'unsigned-byte :channels 1))
(defconstant +16uc2+ (make-image-type :bit-depth 16 :data-type 'unsigned-byte :channels 2))
(defconstant +16uc3+ (make-image-type :bit-depth 16 :data-type 'unsigned-byte :channels 3))
(defconstant +16uc4+ (make-image-type :bit-depth 16 :data-type 'unsigned-byte :channels 4))
;; Signed 16bits
(defconstant +16sc1+ (make-image-type :bit-depth 16 :data-type 'signed-byte :channels 1))
(defconstant +16sc2+ (make-image-type :bit-depth 16 :data-type 'signed-byte :channels 2))
(defconstant +16sc3+ (make-image-type :bit-depth 16 :data-type 'signed-byte :channels 3))
(defconstant +16sc4+ (make-image-type :bit-depth 16 :data-type 'signed-byte :channels 4))
;; Signed 32bits
(defconstant +32sc1+ (make-image-type :bit-depth 32 :data-type 'signed-byte :channels 1))
(defconstant +32sc2+ (make-image-type :bit-depth 32 :data-type 'signed-byte :channels 2))
(defconstant +32sc3+ (make-image-type :bit-depth 32 :data-type 'signed-byte :channels 3))
(defconstant +32sc4+ (make-image-type :bit-depth 32 :data-type 'signed-byte :channels 4))
;; Float 32bits
(defconstant +32fc1+ (make-image-type :bit-depth 32 :data-type 'float :channels 1))
(defconstant +32fc2+ (make-image-type :bit-depth 32 :data-type 'float :channels 2))
(defconstant +32fc3+ (make-image-type :bit-depth 32 :data-type 'float :channels 3))
(defconstant +32fc4+ (make-image-type :bit-depth 32 :data-type 'float :channels 4))
;; Double 64bits
(defconstant +64fc1+ (make-image-type :bit-depth 64 :data-type 'double-float :channels 1))
(defconstant +64fc2+ (make-image-type :bit-depth 64 :data-type 'double-float :channels 2))
(defconstant +64fc3+ (make-image-type :bit-depth 64 :data-type 'double-float :channels 3))
(defconstant +64fc4+ (make-image-type :bit-depth 64 :data-type 'double-float :channels 4))




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
             :type '(unsigned-byte 8))))

(defclass png-image (image)
  ((bit-depth :initarg bit-depth
              :accessor bit-depth
              :type 'unsigned-byte)
   (color-type :initarg color-type
               :accessor color-type
               :type 'symbol)
   (transparency :initarg transparency
                 :accessor transparency
                 :type (or (array * 2) (eql nil)))))

(defclass jpeg-image (image)
  ())

(defclass tiff-image ()
  ((bits-per-sample :accessor bits-per-sample :initarg :bits-per-sample)
   (samples-per-pixel :accessor samples-per-pixel
                      :initarg :samples-per-pixel
                      :initform nil)
   (byte-order :accessor byte-order :initarg :byte-order)
   (color-map :accessor color-map :initarg :color-map :initform nil)
   (min-is-white :accessor min-is-white :initarg :min-is-white
                 :initform nil)))


;(defclass png-image (image))


;(defmethod 
