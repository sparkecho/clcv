(in-package :clcv)

(defun get-random-color ()
  (let ((r (/ (random 256) 256.0))
        (g (/ (random 256) 256.0))
        (b (/ (random 256) 256.0)))
    (xlib:make-color :red r :green g :blue b)))


(defun imshow (width height &optional (host ""))
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
    (xlib:map-window my-window)
    (xlib:event-case (display :force-output-p t
                              :discard-p t)
      (:exposure (count)
                 (when (zerop count)
                   (loop for i from 0 to 400
                      do (let* ((color (get-random-color))
                                (gctex (xlib:create-gcontext
                                        :drawable root-window
                                        :foreground (xlib:alloc-color
                                                     (xlib:window-colormap root-window)
                                                     color)
                                        :background black)))
                           (xlib:draw-point my-window gctex i i))))
                 nil)
      (:button-press () t))
    (xlib:destroy-window my-window)
    (xlib:close-display display)))
