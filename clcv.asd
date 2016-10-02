;;;; clcv.asd

(asdf:defsystem #:clcv
  :name "clcv"
  :description "Common Lisp Computer Vision Library"
  :author "sparkecho <echozhz@126.com>"
  :license "GPL v3.0"
  :serial t
  :depends-on (clx opticl)
  :components
  ((:static-file "README.md")
   (:static-file "LICENSE")
   (:file "package")
   (:file "clcv")
   (:file "core/image")
   (:file "core/swap")
   (:file "imgcodecs/imread")
   (:file "imgcodecs/imwrite")
   (:file "highgui/imshow")))

;; (asdf:defsystem #:core
;;   :depends-on (:clcv)
;;   :components
;;   ((:module "core"
;;             :components
;;             ((:file "package")
;;              (:file "core/image")
;;              (:file "core/swap")))))

;; (asdf:defsystem #:imgcodecs
;;   :depends-on (:opticl :split-sequence)
;;   :components
;;   ((:module "imgcodecs"
;;             :components
;;             ((:file "package")
;;              (:file "imgcodecs/imread")
;;              (:file "imgcodecs/imwrite")))))


;; (asdf:defsystem #:highgui
;;   :depends-on (:mcclim)
;;   :components
;;   ((:module "highgui"
;;             :components
;;             ((:file "package")
;;              (:file "highgui/imshow")))))
