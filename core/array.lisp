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

(defun marray (rows cols &optional (channels 1) (type-specifier :8u) value)
  (let ((element-type (decode-type-specifier type-specifier))
        (dimensions (if (= channels 1)
                        (list rows cols)
                        (list rows cols channels))))
    (typecase value
      (null (make-array dimensions :element-type element-type))
      (number
       (assert (typep value element-type) nil "Initial value ~A is not of given type :~A." value type-specifier)
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
          
