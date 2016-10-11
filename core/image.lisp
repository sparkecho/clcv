;; Basic Data Structures

(in-package :clcv)

(defstruct array-type
  (bit-depth 8 :type unsigned-byte)
  (data-type 'unsigned-byte
             :type (or (eql unsigned-byte) (eql double-float)
                       (eql float) (eql signed-byte)))
  (channels  3 :type (unsigned-byte 8)))

;; Unsigned 8bits
(defconstant +8uc1+ (make-array-type :bit-depth 8 :data-type 'unsigned-byte :channels 1))
(defconstant +8uc2+ (make-array-type :bit-depth 8 :data-type 'unsigned-byte :channels 2))
(defconstant +8uc3+ (make-array-type :bit-depth 8 :data-type 'unsigned-byte :channels 3))
(defconstant +8uc4+ (make-array-type :bit-depth 8 :data-type 'unsigned-byte :channels 4))
;; Signed 8bits
(defconstant +8sc1+ (make-array-type :bit-depth 8 :data-type 'signed-byte :channels 1))
(defconstant +8sc2+ (make-array-type :bit-depth 8 :data-type 'signed-byte :channels 2))
(defconstant +8sc3+ (make-array-type :bit-depth 8 :data-type 'signed-byte :channels 3))
(defconstant +8sc4+ (make-array-type :bit-depth 8 :data-type 'signed-byte :channels 4))
;; Unsigned 16bits
(defconstant +16uc1+ (make-array-type :bit-depth 16 :data-type 'unsigned-byte :channels 1))
(defconstant +16uc2+ (make-array-type :bit-depth 16 :data-type 'unsigned-byte :channels 2))
(defconstant +16uc3+ (make-array-type :bit-depth 16 :data-type 'unsigned-byte :channels 3))
(defconstant +16uc4+ (make-array-type :bit-depth 16 :data-type 'unsigned-byte :channels 4))
;; Signed 16bits
(defconstant +16sc1+ (make-array-type :bit-depth 16 :data-type 'signed-byte :channels 1))
(defconstant +16sc2+ (make-array-type :bit-depth 16 :data-type 'signed-byte :channels 2))
(defconstant +16sc3+ (make-array-type :bit-depth 16 :data-type 'signed-byte :channels 3))
(defconstant +16sc4+ (make-array-type :bit-depth 16 :data-type 'signed-byte :channels 4))
;; Signed 32bits
(defconstant +32sc1+ (make-array-type :bit-depth 32 :data-type 'signed-byte :channels 1))
(defconstant +32sc2+ (make-array-type :bit-depth 32 :data-type 'signed-byte :channels 2))
(defconstant +32sc3+ (make-array-type :bit-depth 32 :data-type 'signed-byte :channels 3))
(defconstant +32sc4+ (make-array-type :bit-depth 32 :data-type 'signed-byte :channels 4))
;; Float 32bits
(defconstant +32fc1+ (make-array-type :bit-depth 32 :data-type 'float :channels 1))
(defconstant +32fc2+ (make-array-type :bit-depth 32 :data-type 'float :channels 2))
(defconstant +32fc3+ (make-array-type :bit-depth 32 :data-type 'float :channels 3))
(defconstant +32fc4+ (make-array-type :bit-depth 32 :data-type 'float :channels 4))
;; Double 64bits
(defconstant +64fc1+ (make-array-type :bit-depth 64 :data-type 'double-float :channels 1))
(defconstant +64fc2+ (make-array-type :bit-depth 64 :data-type 'double-float :channels 2))
(defconstant +64fc3+ (make-array-type :bit-depth 64 :data-type 'double-float :channels 3))
(defconstant +64fc4+ (make-array-type :bit-depth 64 :data-type 'double-float :channels 4))




(defclass image ()
  ((data :initarg :data
         :initform nil
         :accessor data
         :type 'array)
   (width :initarg :width
          :initform 0
          :accessor width
          :type 'unsigned-byte)
   (height :initarg :height
           :initform 0
           :accessor height
           :type 'unsigned-byte)
   (channels :initarg :channels
             :initform 3
             :accessor channels
             :type '(unsigned-byte 8))
   ;; color-models: RGB HSV GRAY YCbCr, etc.
   (color-model :initarg :color-model
                :initform :rgb
                :accessor color-model
                :type 'symbol)))

(defclass jpeg-image (image)
  ())

(defclass png-image (image)
  ((bit-depth :initarg :bit-depth
              :initform 8
              :accessor bit-depth
              :type 'unsigned-byte)
   ;; color-type: (member :greyscale :truecolor :indexed-colour
   ;;                     :greyscale-alpha :truecolor-alpha)
   (color-type :initarg :color-type
               :initform :truecolor
               :accessor color-type
               :type 'symbol)
   (transparency :initarg :transparency
                 :initform nil
                 :accessor transparency
                 :type (or (array * 2) (eql nil)))))

(defclass tiff-image (image)
  ((bits-per-sample :accessor bits-per-sample :initarg :bits-per-sample)
   (samples-per-pixel :accessor samples-per-pixel
                      :initarg :samples-per-pixel
                      :initform nil)
   (byte-order :accessor byte-order :initarg :byte-order)
   (color-map :accessor color-map :initarg :color-map :initform nil)
   (min-is-white :accessor min-is-white :initarg :min-is-white
                 :initform nil)))


;; use the corresponding functions to make a specific type image object.
(defun make-image (filetype width height type)
  (case filetype
    ((:jpeg :jpg) (make-jpeg-image width height type))
    ((:tiff :tif) (make-tiff-image width height type))
    (:png (make-png-image width height type))
    (:pbm (make-pbm-image width height type))
    (:pgm (make-pgm-image width height type))
    (:ppm (make-ppm-image width height type))
    (:gif (make-gif-image width height type))
    (otherwise (error "Unsupported image type ~A~%" filetype))))

(defun make-jpeg-image (width height type)
  (princ "jpeg"))                       ;for test

(defun make-png-image (width height type)
  (let ((bit-depth (array-type-bit-depth type))
        (data-type (array-type-data-type type))
        (channels  (array-type-channels  type)))
    (make-array (list width height channels) :element-type `(,data-type ,bit-depth))))

(defun make-tiff-image (width height type)
  (princ "tiff"))                       ;for test

(defun make-pbm-image (width height type)
  (princ "pbm"))                        ;for test

(defun make-pgm-image (width height type)
  (princ "pgm"))                        ;for test

(defun make-ppm-image (width height type)
  (princ "ppm"))                        ;for test

(defun make-gif-image (width height type)
  (princ "gif"))                        ;for test
