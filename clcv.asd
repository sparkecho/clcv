;;;; clcv.asd

(asdf:defsystem #:clcv
  :name "clcv"
  :description "Common Lisp Computer Vision Library"
  :author "sparkecho <echozhz@126.com>"
  :license "GPL v3.0"
  :serial t
  :depends-on (clx opticl)
  :components ((:static-file "README.md")
               (:static-file "LICENSE")
               (:file "package")
               (:file "clcv")))

(asdf:defsystem #:core
  :depends-on (:clcv)
  :components
  ((:module "core"
            :components
            ((:file "package")
             (:file "core/swap.lisp")))))

(asdf:defsystem #:imgcodecs
  :depends-on (:opticl :split-sequence)
  :components
  ((:module "imgcodecs"
            :components
            ((:file "package")
             (:file "imgcodecs/imread.lisp")
             (:file "imgcodecs/imwrite.lisp")))))


(asdf:defsystem #:highgui
  :depends-on (:mcclim)
  :components
  ((:module "highgui"
            :components
            ((:file "package")
             (:file "highgui/imshow.lisp")))))
