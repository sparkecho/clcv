(uiop/package:define-package :clcv/gui/imshow
    (:use :common-lisp)
  ;; (:shadow #:format)
  (:export #:imshow #:eformat))

(in-package :clcv/gui/imshow)


(defun mats-width (mats)
  (if (null mats)
      (error "No matrices in mats, mats is empty.")
      (array-dimension (car mats) 1)))

(defun mats-height (mats)
  (if (null mats)
      (error "No matrices in mats, mats is empty.")
      (array-dimension (car mats) 0)))


(defun put-pixel (renderer x y &optional (r 255) (g 255) (b 255) (a 255))
  (sdl2:set-render-draw-color renderer r g b a)
  (sdl2:render-draw-point renderer x y))


(defun imshow (mats &optional (title ""))
  (let ((height (mats-height mats))
        (width (mats-width mats))
        (mat1 (first mats))
        (mat2 (second mats))
        (mat3 (third mats)))
    (sdl2:with-init (:video)
      (sdl2:with-window (win :title title :w width :h height :flags '(:shown))
        (sdl2:with-renderer (renderer win :flags '(:renderer-accelerated))
          (dotimes (x width)
            (dotimes (y height)
              (put-pixel renderer x y (aref mat1 y x) (aref mat2 y x) (aref mat3 y x))))
          (sdl2:render-present renderer)
          (sdl2:with-event-loop (:method :poll)
            (:keyup
             (:keysym keysym)
             (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
               (sdl2:push-event :quit)))
            (:idle
             ()
             (sdl2:delay 300))
            (:quit () t)))))))


(defmacro eformat (destination control-string &rest format-arguments)
  `(if (and ,destination (listp ,destination))
       (imshow ,destination (cl:format nil ,control-string ,@format-arguments))
       (cl:format ,destination ,control-string ,@format-arguments)))



#|
(defun imread (path)
  (let* ((image (opticl:read-image-file path))
         (dims (array-dimensions image))
         (rows (first dims))
         (cols (second dims))
         (channels (third dims)))
    (loop for k from 0 below channels
       collect (let ((mat (matrix rows cols :type '(unsigned-byte 8))))
                 (with-matrix (e mat)
                   (setf e (aref image i j k)))
                 mat))))


(defun mats-height (mats)
  (matrix-rows (car mats)))

(defun mats-width (mats)
  (matrix-cols (car mats)))


(defun put-pixel (renderer x y &optional (r 255) (g 255) (b 255) (a 255))
  (sdl2:set-render-draw-color renderer r g b a)
  (sdl2:render-draw-point renderer x y))


(defun imshow (mats &optional (title ""))
  (let ((height (mats-height mats))
        (width (mats-width mats))
        (data1 (matrix-data (first  mats)))
        (data2 (matrix-data (second mats)))
        (data3 (matrix-data (third  mats))))
    (sdl2:with-init (:video)
      (sdl2:with-window (win :title title :w width :h height :flags '(:shown))
        (sdl2:with-renderer (renderer win :flags '(:renderer-accelerated))
          (dotimes (i height)
            (dotimes (j width)
              (put-pixel renderer j i (aref data1 i j) (aref data2 i j) (aref data3 i j))))
          (sdl2:render-present renderer)
          (sdl2:with-event-loop (:method :poll)
            (:keyup
             (:keysym keysym)
             (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
               (sdl2:push-event :quit)))
            (:idle
             ())
            (:quit () t)))))))


(defun show-image-file (path)
  path)
|#
