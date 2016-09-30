(in-package :clcv)

(use-package :split-sequence)
(use-package :opticl)

(defun imread (pathname)
  (read-image-file pathname))
