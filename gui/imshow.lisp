(uiop/package:define-package :clcv/gui/imshow
    (:use :common-lisp)
  (:export #:imshow))

(in-package :clcv/gui/imshow)


(declaim (inline put-pixel))
(defun put-pixel (renderer x y r g b a)
  (sdl2:set-render-draw-color renderer r g b a)
  (sdl2:render-draw-point renderer x y))


(defun imshow (image &key (title "") (delay-time 300))
  "Display the given image on the screen."
  (sdl2:with-init (:video)
    (opticl:with-image-bounds (height width) image
      (sdl2:with-window (window :title title :w width :h height :flags '(:shown))
        (sdl2:with-renderer (renderer window :flags '(:renderer-accelerated))          
          (dotimes (y height)
            (dotimes (x width)
              (multiple-value-bind (r g b a)
                  (opticl:pixel image y x)
                (put-pixel renderer x y r (or g 0) (or b 0) (or a 255)))))
          (sdl2:render-present renderer)
          (sdl2:with-event-loop (:method :poll)
            (:keyup
             (:keysym keysym)
             (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
               (sdl2:push-event :quit)))
            (:idle
             ()
             (sdl2:delay delay-time))
            (:quit () t)))))))
