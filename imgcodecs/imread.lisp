(in-package :clcv)

;; Returns an image object
(defun imread (filename)
  (let* ((pathname (pathname filename))
         (suffix   (pathname-type pathname)) ;file suffix name
         (filetype (intern (string-upcase suffix) :keyword))) ;unique filetype
    (make-image filetype)))
