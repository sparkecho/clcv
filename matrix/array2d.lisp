;;;; 2 dimensions array based matrix version
(uiop/package:define-package :clcv/matrix/array2d
  (:use :common-lisp)
  (:export #:matrix
           #:m+
           #:m-
           #:m*
           #:diag
           #:diagp
           #:eye
           #:eyep
           #:copy-matrix
           #:pr
           #:print-matrix
           #:mexpt
           #:trans
           #:row-multiplyf
           #:row-addf
           #:row-switchf
           #:col-multiplyf
           #:col-addf
           #:col-switchf
           #:rearrangef
           #:row-echelon
           #:row-canonical
           #:col-canonical
           #:canonical
           #:row-rank
           #:col-rank
           #:rank
           #:inversion
           #:det
           #:submatrix
           #:minor
           #:cofactor
           #:signed-minor
           #:adj
           #:inv
           #:tr
           #:dot
           #:mapeach
           #:msum
           #:norm
           #:euclidean-distance))

(in-package #:clcv/matrix/array2d)


;; 矩阵构造函数
;; Constructor function of matrix
(defun matrix (rows cols &optional initvec)
  (let ((mat (make-array (list rows cols) :initial-element 0)))
    (when initvec
        (loop for i from 0 below rows
           do (loop for j from 0 below cols
                 do (let ((k (+ (* i cols) j)))
                      (setf (aref mat i j) (svref initvec k))))))
    mat))


;; 构造以给定向量中元素为对角线元素的对角矩阵
;; Build a diagonal matrix which has all elements in the given vector
(defun diag (initvec)
  (let* ((order (length initvec))
         (result (matrix order order)))
    (loop for i from 0 below order
       do (setf (aref result i i) (aref initvec i)))
    result))


;; 判断是否是对角矩阵的谓词
;; Predicate of if the given matrix is a diag matrix
(defun diagp (mat)
  (let ((rows (array-dimension mat 0))
        (cols (array-dimension mat 1)))
    (loop for i from 0 below rows
       do (unless (loop for j from 0 below cols
                     do (when (and (/= (aref mat i j) 0) (/= i j))
                          (return nil))
                     finally (return t))
            (return nil))
       finally (return t))))


;; 单位矩阵构造函数
;; Build a `n order' eye matrix (identity matrix)
(defun eye (n)
  (let ((mat (matrix n n)))
    (loop for i from 0 below n
       do (loop for j from 0 below n
             do (if (= i j)
                    (setf (aref mat i j) 1)
                    (setf (aref mat i j) 0))))
    mat))


;; 判断是否是单位矩阵的谓词
;; Predicate of if the given matrix is a eye matrix
(defun eyep (mat)
  (let ((rows (array-dimension mat 0))
        (cols (array-dimension mat 1)))
    (and (= rows cols)
         (> rows 0)
         (> cols 0)
         (loop for i from 0 below rows
            do (unless (loop for j from 0 below cols
                          do (when (if (= i j)
                                       (/= (aref mat i j) 1)
                                       (/= (aref mat i j) 0))
                               (return nil))
                          finally (return t))
                 (return nil))
            finally (return t)))))


;; Build a new matrix with the same content of mat
(defun copy-matrix (mat)
  (let* ((rows (array-dimension mat 0))
         (cols (array-dimension mat 1))
         (copy (matrix rows cols)))
    (loop for i from 0 below rows
       do (loop for j from 0 below cols
             do (setf (aref copy i j) (aref mat i j))))
    copy))


;; 矩阵美观打印函数
;; Matrix pretty print
(defun print-matrix (mat)
  (let ((rows (array-dimension mat 0))
        (cols (array-dimension mat 1)))
    (loop for i from 0 below rows
       do (progn (loop for j from 0 below cols
                    do (format t "~A~A" #\Tab (aref mat i j)))
                 (format t "~%")))))


;; print-matrix 的别名
;; Nickname of print-matrix
(defun pr (mat) (print-matrix mat))


;; 二元矩阵加法运算
;; Binary matrix addition
(defun madd (mat1 mat2)
  (let ((rows1 (array-dimension mat1 0))
        (cols1 (array-dimension mat1 1))
        (rows2 (array-dimension mat2 0))
        (cols2 (array-dimension mat2 1)))
    (assert (and (= rows1 rows2) (= cols1 cols2)))
    (let ((mat3 (matrix rows1 cols1)))
      (loop for i from 0 below rows1
         do (loop for j from 0 below cols1
               do (setf (aref mat3 i j)
                        (+ (aref mat1 i j) (aref mat2 i j)))))
      mat3)))


;; 拓展的矩阵加法运算
;; Extended matrix addition
;; m+ can add n matrices together (n >= 1)
(defun m+ (mat &rest mats)
  (reduce #'madd (cons mat mats)))


;; 矩阵二元减法运算
;; Binary subtraction of matrix
(defun msub (mat1 mat2)
  (let ((rows1 (array-dimension mat1 0))
        (cols1 (array-dimension mat1 1))
        (rows2 (array-dimension mat2 0))
        (cols2 (array-dimension mat2 1)))
    (assert (and (= rows1 rows2) (= cols1 cols2)))
    (let ((mat3 (matrix rows1 cols1)))
      (loop for i from 0 below rows1
         do (loop for j from 0 below cols1
               do (setf (aref mat3 i j)
                        (- (aref mat1 i j) (aref mat2 i j)))))
      mat3)))


;; 矩阵取负(反)运算
;; Build a new matrix with each element negative of the corresponding one in mat
(defun mminus (mat)
  (let* ((rows (array-dimension mat 0))
         (cols (array-dimension mat 1))
         (result (matrix rows cols)))
    (loop for i from 0 below rows
       do (loop for j from 0 below cols
             do (setf (aref result i j) (- (aref mat i j)))))
    result))


;; 拓展的矩阵减法
;; Extended subtraction of matrix
;; When there is only one parameter passed, call function #'mminus instead of #'msub
(defun m- (mat &rest mats)
  (if (null mats)
      (mminus mat)
      (reduce #'msub (cons mat mats))))


;; 二元矩阵乘法运算
;; Binary multiplication of matrix
(defun mmul (mat1 mat2)
  (let ((rows1 (array-dimension mat1 0))
        (cols1 (array-dimension mat1 1))
        (rows2 (array-dimension mat2 0))
        (cols2 (array-dimension mat2 1)))
    (assert (= cols1 rows2))
    (let ((result (matrix rows1 cols2)))
      (loop for i from 0 below rows1
         do (loop for j from 0 below cols2
               do (setf (aref result i j)
                        (loop for k from 0 below cols1
                           sum (* (aref mat1 i k) (aref mat2 k j))))))
      (if (and (= rows1 1) (= cols2 1))
          (aref result 0 0)
          result))))


;; 矩阵数乘运算
;; Compute a matrix multiplied by a number
(defun nmul (num mat)
  (let* ((rows (array-dimension mat 0))
         (cols (array-dimension mat 1))
         (result (matrix rows cols)))
    (loop for i from 0 below rows
       do (loop for j from 0 below cols
             do (setf (aref result i j) (* num (aref mat i j)))))
    result))


;; 混合二元乘法, 两个参数相乘, 每个参数要么是数字要么是矩阵
;; Mixed binary multiplication of matrix
;; The two parameters are both required either a number or a matrix
(defun mix* (num/mat1 num/mat2)
  (cond ((numberp num/mat1) (cond ((numberp num/mat2) (* num/mat1 num/mat2))
                                  (t (nmul num/mat1 num/mat2))))
        ((numberp num/mat2) (nmul num/mat2 num/mat1))
        (t (mmul num/mat1 num/mat2))))


;; 拓展的矩阵乘法
;; Extended multiplication of matrix
;; Function #'m* can take more than one parameter, and each of them should be
;; either a number or a matrix
(defun m* (num/mat &rest nums/mats)
  (reduce #'mix* (cons num/mat nums/mats)))


;; 矩阵乘方(矩阵幂运算)
;; Exponentiation of matrix
(defun mexpt (mat power)
  (let ((rows (array-dimension mat 0))
        (cols (array-dimension mat 1)))
    (assert (= rows cols))
    (let ((result (eye rows)))
      (dotimes (i power)
        (setf result (m* result mat)))
      result)))


;; 开平方
;; 结果中的每个元素为原矩阵中对应元素的平方根
;; Square root of the matrix
;; Each element of the result is square root of the corresponding element
;; of the origin matrix
(defun msqrt (mat)
  (let* ((rows (array-dimension mat 0))
         (cols (array-dimension mat 1))
         (result (matrix rows cols)))
    (loop for i from 0 below rows
       do (loop for j from 0 below cols
             do (setf (aref result i j)
                      (sqrt (aref mat i j)))))
    result))


;; 矩阵转置
;; Matrix Transposion
(defun trans (mat)
  (let* ((rows (array-dimension mat 0))
         (cols (array-dimension mat 1))
         (result (matrix cols rows)))
    (loop for i from 0 below rows
       do (loop for j from 0 below cols
             do (setf (aref result j i) (aref mat i j))))
    result))  


;;; 初等行变换
;;; Elementary row operation

;; 倍法变换
;; Row multiplication
(defun row-multiplyf (mat i k)
  (let ((cols (array-dimension mat 1)))
    (loop for col from 0 below cols
       do (setf (aref mat i col) (* k (aref mat i col))))
    mat))


;; 消法变换
;; Row addition
(defun row-addf (mat i j k)
  (let ((cols (array-dimension mat 1)))
    (loop for col from 0 below cols
       do (incf (aref mat i col) (* k (aref mat j col))))
    mat))


;; 换法变换
;; Row switching
(defun row-switchf (mat i j)
  (let ((cols (array-dimension mat 1)))
    (loop for col from 0 below cols
       do (rotatef (aref mat i col) (aref mat j col)))
    mat))
  

;;; 初等行变换
;;; Elementary row operation

;; 倍法变换
;; Col multiplication
(defun col-multiplyf (mat i k)
  (let ((rows (array-dimension mat 0)))
    (loop for row from 0 below rows
       do (setf (aref mat row i) (* k (aref mat row i))))
    mat))


;; 消法变换
;; Col addition
(defun col-addf (mat i j k)
  (let ((rows (array-dimension mat 0)))
    (loop for row from 0 below rows
       do (incf (aref mat row j) (* k (aref mat row i))))
    mat))


;; 换法变换
;; Col switching
(defun col-switchf (mat i j)
  (let ((rows (array-dimension mat 0)))
    (loop for row from 0 below rows
       do (rotatef (aref mat row i) (aref mat row j)))
    mat))
  

;; 统计矩阵第 row 行中前导的 0 的个数
;; Aux function
;; Count how many zeros are there in row-th row before a non-zero element
(defun count-prefix-zeros (mat row)
  (let ((cnt 0))
    (dotimes (col (array-dimension mat 1))
      (if (zerop (aref mat row col))
          (incf cnt)
          (return cnt)))
    cnt))


;; 将矩阵按照每行前导的 0 的个数的升序重排行
;; Aux function
;; Rearrange the matrix by quantity of prefixed zeros
(defun rearrangef (mat)
  (let* ((rows (array-dimension mat 0))
         (nums (make-array rows)))
    (dotimes (i rows)
      (setf (aref nums i) (count-prefix-zeros mat i)))
      (loop for k from 0 below rows
         do (loop for i from 1 below rows
               do (let ((j (- i 1)))
                    (when (< (aref nums i) (aref nums j))
                      (rotatef (aref nums i) (aref nums j))
                      (row-switchf mat i j)))))
      mat))


;; 化为行阶梯矩阵
;; Gaussian elimination (row reduction)
;; Row echelon form
(defun row-echelon (mat)
  (let ((tmat (copy-matrix mat)))
    (rearrangef tmat)
    (let ((rows (array-dimension mat 0))
          (cols (array-dimension mat 1)))
      (loop for i from 0 below (1- rows)
         do (let ((pos (count-prefix-zeros tmat i)))
              (loop for j from (1+ i) below rows
                 do (when (/= pos cols)
                      (when (/= (aref tmat j pos) 0)
                        (row-addf tmat j i (- (/ (aref tmat j pos) (aref tmat i pos)))))))))
      tmat)))


;; 化为行最简矩阵 (行规范型矩阵)
;; Reduced row echelon form / row canonical form
(defun row-canonical (mat)
  (let ((tmat (row-echelon mat))
        (rows (array-dimension mat 0))
        (cols (array-dimension mat 1)))
    (loop for j from (1- rows) downto 1
       do (let ((pos (count-prefix-zeros tmat j)))
            (when (/= pos cols)
              (row-multiplyf tmat j (/ 1 (aref tmat j pos)))
              (loop for i from (1- j) downto 0
                 do (when (/= (aref tmat i pos) 0)
                      (row-addf tmat i j (- (/ (aref tmat i pos) (aref tmat j pos)))))))))
    tmat))


;; 化为列最简矩阵
;; Reduced col echelon form / col canonical form
(defun col-canonical (mat)
  (trans (rearrangef (row-canonical (trans mat)))))


;; 化为标准型矩阵
;; Transform the mat to canonical form
(defun canonical (mat)
  (col-canonical (row-canonical mat)))


;; 矩阵的行秩
;; Row rank of matrix
(defun row-rank (mat)
  (let* ((tmat (row-echelon mat))
         (rows (array-dimension mat 0))
         (cols (array-dimension mat 1))
         (rank rows))
    (loop for i from (1- rows) downto 0
       do (let ((flag t))
            (loop for j from (1- cols) downto 0
               do (when (/= (aref tmat i j) 0)
                    (setf flag nil)
                    (return)))
            (when flag (decf rank))))
    rank))


;; 矩阵的列秩
;; Col rank of matrix
(defun col-rank (mat)
  (row-rank (trans mat)))


;; 矩阵的秩
;; Rank fo matrix
(defun rank (mat)
  (min (row-rank (row-canonical mat))
       (row-rank (row-canonical (trans mat)))))
               

;; 逆序数
;; Compute the inversion of the vector vec's permutation
(defun inversion (vec)
  (let ((len (length vec))
        (cnt 0))
    (loop for i from 0 below (1- len)
       do (loop for j from (1+ i) below len
             do (when (< (svref vec j) (svref vec i))
                  (incf cnt))))
    cnt))


;; 生成全排列
;; 根据给定的数组生成包含该数组的所有全排列的列表
;; Generate permutation
;; Generate a list that includes all the permutations of the given vector
(defun permutation (vec)
  (let ((result nil))
    (labels ((perm (vec k len)
               (if (= k len)
                   (push (copy-seq vec) result)
                   (loop for i from k below len
                      do (progn (rotatef (aref vec i) (aref vec k))
                                (perm vec (1+ k) len)
                                (rotatef (aref vec k) (aref vec i)))))))
      (perm vec 0 (length vec)))
    result))


;; 生成一个从 0 到 n-1 的顺序数组
;; Generate an ordered vector includes numbers that from 0 to n-1
(defun gen-seq-vec (n)
  (make-array n :initial-contents (loop for i from 0 below n collect i)))


;; 行列式
;; Compute the determinant of a square matrix
(defun det (mat)
  (let ((rows (array-dimension mat 0))
        (cols (array-dimension mat 1)))
  (assert (= rows cols))
  (let ((permutations (permutation (gen-seq-vec rows)))
        (acc 0))
    (dolist (perm permutations)
      (let ((mul 1))                    ;multiplicative
        (dotimes (i rows)
          (setf mul (* mul (aref mat i (svref perm i)))))
        (incf acc (* mul (expt -1 (inversion perm))))))
    acc)))


;; 生成除第 i 行和第 j 行元素的子矩阵
;; Generate the submatrix of the matrix mat that exclude i-th row and j-th colum elements
(defun submatrix (mat i j)
  (let* ((rows (array-dimension mat 0))
         (cols (array-dimension mat 1))
         (result (matrix (1- rows) (1- cols))))
    (loop for r from 0 below (1- rows)
       do (if (< r i)              
              (loop for c from 0 below (1- cols)
                 do (if (< c j)
                        (setf (aref result r c) (aref mat r c))
                        (setf (aref result r c) (aref mat r (1+ c)))))
              (loop for c from 0 below (1- cols)
                 do (if (< c j)
                        (setf (aref result r c) (aref mat (1+ r) c))
                        (setf (aref result r c) (aref mat (1+ r) (1+ c)))))))
    result))


;; 余子式
;; Minor
(defun minor (mat i j)
  (det (submatrix mat i j)))


;; 代数余子式
;; Signed minor of a matrix
(defun cofactor (mat i j)
  (* (expt -1 (+ i j))
     (minor mat i j)))


(defun signed-minor (mat i j)
  (cofactor mat i j))


;; 伴随矩阵
;; Adjugate
(defun adj (mat)
  (let* ((order (array-dimension mat 0))
         (result (matrix order order)))
    (loop for i from 0 below order
       do (loop for j from 0 below order
             do (setf (aref result i j)
                      (cofactor mat j i))))
    result))


;; 矩阵的逆
;; Inverse of matrix
(defun inv (mat)
  (assert (/= (det mat) 0))
  (m* (/ 1 (det mat)) (adj mat)))


;; 矩阵的迹
;; Trace of matrix
(defun tr (mat)
  (assert (= (array-dimension mat 0) (array-dimension mat 1)))
  (let ((order (array-dimension mat 0)))
    (loop for i from 0 below order sum (aref mat i i))))


;; 计算向量的内积(数量积)
;; Compute inner product(scalar product) of two vector
(defun dot (vec1 vec2)
  (let ((rows1 (array-dimension vec1 0))
        (cols1 (array-dimension vec1 1))
        (rows2 (array-dimension vec2 0))
        (cols2 (array-dimension vec2 1)))
    (assert (and (= (min rows1 cols1) 1)
                 (= (min rows2 cols2) 1)
                 (= (max rows1 cols1) (max rows2 cols2))))
    (let ((v1 (if (= rows1 1)
                  vec1
                  (trans vec1)))
          (v2 (if (= cols2 1)
                  vec2
                  (trans vec2))))
      (m* v1 v2))))


;; 对矩阵的每个元素进行操作
;; Do the given function on each element of mat
(defun mapeach (function mat)
  (let* ((rows (array-dimension mat 0))
         (cols (array-dimension mat 1))
         (result (matrix rows cols)))
    (loop for i from 0 below rows
       do (loop for j from 0 below cols
             do (setf (aref result i j)
                      (funcall function (aref mat i j)))))
    result))


;; 计算矩阵所有元素之和
;; Compute the sum of all elements of the given mat
(defun msum (mat)
  (let ((rows (array-dimension mat 0))
        (cols (array-dimension mat 1)))
    (loop for i from 0 below rows
       sum (loop for j from 0 below cols
                sum (aref mat i j)))))    


;; p-范数
;; Compute p-norm of vector vec
(defun norm (vec &optional (p 2))
  (assert (or (= (array-dimension vec 0) 1)
              (= (array-dimension vec 1) 1)))
  (expt (msum (mapeach #'(lambda (x) (expt (abs x) p)) vec))
        (/ 1 p)))


;; 欧几里得距离(欧式距离)
;; Euclidean distance of two vectors
(defun euclidean-distance (vec1 vec2)
  (assert (and (= (array-dimension vec1 0) (array-dimension vec2 0))
               (= (array-dimension vec1 1) (array-dimension vec2 1))))
  (let ((vec (m- vec1 vec2)))
    (norm vec)))
