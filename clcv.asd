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

