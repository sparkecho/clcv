(uiop/package:define-package :clcv/core/array
    (:use :common-lisp)
  (:export #:marray
           #:reshape
           #:copy-array
           #:copy-to))


(defun decode-type-specifier (type-specifier)
  (let* ((str (symbol-name type-specifier))
         (end (1- (length str))))
    (case (char str end)
      (#\U (if (> end 0)
               (list 'unsigned-byte (parse-integer str :start 0 :end end))
               'unsigned-byte))
      (#\S (if (> end 0)
               (list 'signed-byte (parse-integer str :start 0 :end end))
               'signed-byte))
      (#\F (if (> end 0)
               (progn
                 (assert (= (1- end) 0) nil "Unsupported type specifier ~A." type-specifier)
                 (case (char str (1- end))
                   (#\S 'single-float)
                   (#\D 'double-float)
                   (otherwise (error "Unsupported type specifier ~A." type-specifier))))
                 'float))
      (otherwise (error "Unsupported type specifier ~A." type-specifier)))))

(defun array-type (arr)
  (let ((etype (array-element-type arr)))
    (if (listp etype)
        (intern (format nil "~A~A"
                        (second etype)
                        (ecase (first etype)
                          (unsigned-byte #\U)
                          (signed-byte #\S)))
                "KEYWORD")
        (ecase etype
          (float :f)
          (single-float :sf)
          (double-float :df)))))

(defun marray (rows cols &optional (channels 1) (type-specifier :8u) value)
  (%marray rows cols channels (decode-type-specifier type-specifier) value))

(defun %marray (rows cols channels element-type value)
  (let ((dimensions (if (= channels 1)
                        (list rows cols)
                        (list rows cols channels))))
    (typecase value
      (null (make-array dimensions :element-type element-type))
      (number
       (assert (typep value element-type) nil "Initial value ~A is not of given type :~A." value element-type)
       (make-array dimensions
                   :element-type element-type
                   :initial-element value))
      (vector
       (assert (> channels 1) nil "You should use this form of initialization only when channels >= 2.")
       (assert (= (length value) channels) nil "Initial value vector's size does not match channel number.")
       (let* ((arr (make-array dimensions :element-type element-type))
              (vec (array-storage-vector arr))
              (size (* rows cols channels)))
         ;; (dotimes (i channels)
         ;;   (let ((val (aref value i)))
         ;;     (loop for j from i below size by channels
         ;;        do (setf (aref vec j) val))))
         (loop for i below channels
            for val = (aref value i)
            do (loop for j from i below size by channels
                  do (setf (aref vec j) val)))
         arr))
      (list (assert (= (length value) channels) nil "Initial value list's size does not match channel number.")
            (let* ((arr (make-array dimensions :element-type element-type))
                   (vec (array-storage-vector arr))
                   (size (* rows cols channels)))
              (loop for i below channels
                 for val = (pop value)
                 do (loop for j from i below size by channels
                       do (setf (aref vec j) val)))
              arr))
      (otherwise (error "Unsupported initial form.")))))

(defun reshape (arr rows cols &optional (channels 1))
  (assert (= (array-total-size arr) (* rows cols channels)) nil "Number of elements does not equal.")
  (let ((dimensions (cond ((and (= rows 1) (= channels 1)) cols)
                          ((= channels 1) (list rows cols))
                          ((= rows 1) (list cols channels))
                          (t (list rows cols channels)))))
    (make-array dimensions :element-type (array-element-type arr)
                :displaced-to arr)))

(defun copy-array (arr)
  (let* ((val (make-array (array-dimensions arr)
                          :element-type (array-element-type arr))))
    (loop for i below (array-total-size arr)
       do (setf (row-major-aref val i) (row-major-aref arr i)))
    val))

;; Add error check for parameter's dimensions
(defun copy-to (src dst &optional mask)
  (if (null mask)
      (loop for i below (array-total-size src)
         do (setf (row-major-aref dst i) (row-major-aref src i)))
      (cond ((= (array-rank src) 2)
             (loop for i below (array-total-size src)
                do (setf (row-major-aref dst i)
                         (logand (row-major-aref src i)
                                 (row-major-aref mask i)))))
            ((= (array-rank src) 3)
             (loop for i below (array-dimension src 0)
                do (loop for j below (array-dimension src 1)
                      do (loop for k below (array-dimension src 2)
                            do (setf (aref dst i j k)
                                     (logand (aref src i j k) (aref mask i j)))))))
            (t (error "Array has a rank > 3 is not supported right now.")))))

(defun channels (arr)
  (if (< (array-rank arr) 3)
      1
      (array-dimension arr 2)))

(defun cross (arr))
(defun dot (arr))
(defun diag (arr))

(defun for-each (arr fn)
  (let ((vec (array-storage-vector arr)))))

(defun resize (arr))
(defun row (arr))
(defun col (arr))




(defclass array2d ()
  ((rows :initarg :rows :reader rows :type unsigned-byte)
   (cols :initarg :cols :reader cols :type unsigned-byte)
   (channels :initform 1 :reader channels)
   (data :initarg :data :accessor data :type (simple-array * *))))

(defclass array3d ()
  ((rows :initarg :rows :reader rows :type unsigned-byte)
   (cols :initarg :cols :reader cols :type unsigned-byte)
   (channels :initarg :channels :reader channels :type unsigned-byte)
   (data :initarg :data :accessor data :type (simple-array * *))))

(defgeneric ref (array i &optional j k))

(defmethod ref ((array array2d) i &optional j k)
  (declare (type unsigned-byte i))
  (cond ((null j) (aref (data array) i))
        ((null k) (aref (data array) (+ (* i (cols array)) j)))
        (t (error "You are accessing a two dimensional array, but 3 indices provided."))))

(defmethod ref ((array array3d) i &optional j k)
  (declare (type unsigned-byte i))
  (cond ((null j) (aref (data array) i))
        ((null k) (aref (data array) (+ (* i (cols array) (channels array)) j)))
        (t (aref (data array) (+ (* i (cols array) (channels array)) (* j (channels array)) k)))))
