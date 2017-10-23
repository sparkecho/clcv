(uiop/package:define-package :clcv/gui/imshow
    (:use :common-lisp)
  (:import-from :sdl2
                #:with-init
                #:with-window
                #:with-renderer
                #:set-render-draw-color
                #:render-draw-point
                #:render-present
                #:with-event-loop
                #:scancode=
                #:scancode-value
                #:push-event
                #:delay)
  (:import-from :opticl
                #:with-image-bounds
                #:pixel)
  (:export #:imshow))

(in-package :clcv/gui/imshow)


(declaim (inline put-pixel))
(defun put-pixel (renderer x y r g b a)
  (set-render-draw-color renderer r g b a)
  (render-draw-point renderer x y))


(defun imshow (image &key (title "") (delay-time 300) (display-channel t))
  "Display the given image on the screen."
  (bt:make-thread
   (lambda ()
     (with-init (:video)
       (with-image-bounds (height width channels) image
         (with-window (window :title title :w width :h height :flags '(:shown))
           (with-renderer (renderer window :flags '(:renderer-accelerated))
             (cond
               ;; gray scale image
               ((null channels)
                (case display-channel
                  (:r (dotimes (y height)
                        (dotimes (x width)
                          (let ((grayscale (pixel image y x)))
                            (put-pixel renderer x y grayscale 0 0 255)))))
                  (:g (dotimes (y height)
                        (dotimes (x width)
                          (let ((grayscale (pixel image y x)))
                            (put-pixel renderer x y 0 grayscale 0 255)))))
                  (:b (dotimes (y height)
                        (dotimes (x width)
                          (let ((grayscale (pixel image y x)))
                            (put-pixel renderer x y 0 0 grayscale 255)))))
                  (:a (dotimes (y height)
                        (dotimes (x width)
                          (let ((grayscale (pixel image y x)))
                            (put-pixel renderer x y 0 0 0 grayscale)))))
                  (otherwise (dotimes (y height)
                               (dotimes (x width)
                                 (let ((grayscale (pixel image y x)))
                                   (put-pixel renderer x y
                                              grayscale grayscale grayscale 255)))))))
               ;; rgb image / rgba image
               (t (dotimes (y height)
                    (dotimes (x width)
                      (multiple-value-bind (r g b a)
                          (pixel image y x)
                        (put-pixel renderer x y r g b (or a 255)))))))
             (render-present renderer)
             (with-event-loop (:method :poll)
               (:keyup
                (:keysym keysym)
                (when (scancode= (scancode-value keysym) :scancode-escape)
                  (push-event :quit)))
               (:idle
                ()
                (delay delay-time))
               (:quit () t)))))))))
