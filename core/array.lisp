(uiop/package:define-package :clcv/core/array
    (:use :common-lisp)
  (:export #:arr))

(defun decode-type-specifier (type)
  (let* ((str (symbol-name type))
         (len (length str))
         (pos (or (position #\C str) len))
         (bit (and (> pos 1) (parse-integer (subseq str 0 (1- pos)))))
         (typ (case (char str (1- pos))
                (#\U 'unsigned-byte)
                (#\S 'signed-byte)
                (#\F 'single-float)
                (#\D 'double-float)
                (otherwise (error "Unsupported type specifier character ~A" (char str (1- pos))))))
         (chn (if (= pos len)
                  1
                  (parse-integer (subseq str (1+ pos))))))
    (assert (> chn 0) nil "Channel number should be a positive integer, but ~A is provided." chn)
    (when (and (member typ '(single-float double-float)) bit)
      (error "Float type does not support specifying bit length, but ~A is provided." bit))
    (if (null bit)
        (list chn typ)
        (progn
          (assert (> bit 0) nil "Bit length should be a positive integer, but ~A is provided." bit)
          (list chn typ bit)))))

(defmethod arr (rows cols type (value number))
  (let ((info (decode-type-specifier type)))
    (if (= (car info) 1)
        (make-array (list rows cols) :element-type (cdr info) :initial-element value)
        (make-array (list rows cols (car info)) :element-type (cdr info) :initial-element value))))

(defmethod arr (rows cols type (value vector))
  (let* ((info (decode-type-specifier type))
         (channels (car info)))
    (assert (> channels 1) nil "There is only one channel, you should use a number to initiaize the array.")
    (let ((arr (make-array (list rows cols channels) :element-type (cdr info))))
      (loop for k below channels
         do (loop for i below rows
               do (loop for j below cols
                     do (setf (aref arr i j k) (aref value k)))))
      arr)))

(defun reshape (arr rows cols)
  (assert (= (array-total-size arr) (* rows cols)) nil "Number of elements does not equal.")
  (if (= rows 1)
      (make-array cols :element-type (array-element-type arr)
                  :displaced-to arr)
      (make-array (list rows cols) :element-type (array-element-type arr)
                  :displaced-to arr)))

(defun copy-arr (arr)
  (let* ((val (make-array (array-dimensions arr)
                          :element-type (array-element-type arr))))
    (loop for i below (array-total-size arr)
       do (setf (row-major-aref val i) (row-major-aref arr i)))
    val))

(defun copy-to (src dst &optional mask)
  (if (null mask)
      (loop for i below (array-total-size src)
         do (setf (row-major-aref dst i) (row-major-aref src i)))
      ))
