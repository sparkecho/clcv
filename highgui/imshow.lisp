(in-package :clcv)

(defun make-color (red green blue)
  (let ((r (/ red 256.0))
        (g (/ green 256.0))
        (b (/ blue 256.0)))
    (xlib:make-color :red r :green g :blue b)))

;; image-data is a 3 dimentions array
;; example:
;; (clcv:imshow 227 246 (opticl:read-png-file "~/Picture/cat.png"))
;; (clcv:imshow 600 400 (opticl:read-jpeg-file "~/Picture/flower.jpg"))
(defun imshow (winname width height image-data &optional (host ""))
  (let* ((display (xlib:open-display host))
         (screen (first (xlib:display-roots display)))
         (black (xlib:screen-black-pixel screen))
         (root-window (xlib:screen-root screen))
         (my-window (xlib:create-window
                     :parent root-window
                     :x 0
                     :y 0
                     :width width
                     :height height
                     :background black
                     :event-mask (xlib:make-event-mask :exposure
                                                       :button-press))))
    (xlib:change-property my-window
                          :wm_name
                          winname
                          :string
                          8
                          :transform #'char-code)
    (xlib:map-window my-window)
    (xlib:event-case (display :force-output-p t
                              :discard-p t)
      (:exposure (count)
                 (when (zerop count)
                   (loop for i from 0 below width
                      do (loop for j from 0 below height
                            do (let* ((color (make-color (aref image-data j i 0)
                                                         (aref image-data j i 1)
                                                         (aref image-data j i 2)))
                                      (gctex (xlib:create-gcontext
                                              :drawable root-window
                                              :foreground (xlib:alloc-color
                                                           (xlib:window-colormap root-window)
                                                           color)
                                              :background black)))
                                 (xlib:draw-point my-window gctex i j)))))
                 nil)
      (:button-press () t))
    (xlib:destroy-window my-window)
    (xlib:close-display display)))
