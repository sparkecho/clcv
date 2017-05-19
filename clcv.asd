;;;; clcv.asd

#-asdf3.1 (error "clcv requires ASDF 3.1")
(asdf:defsystem "clcv"
  :name "clcv"
  :description "Common Lisp Computer Vision Library"
  :author "sparkecho <echozhz@126.com>"
  :license "GPL v3.0"
  :class :package-inferred-system
  :depends-on ("opticl"
               "sdl2"
               "clcv/core/all"
               "clcv/matrix/all"
               "clcv/dip/all"
               "clcv/gui/all"
               "clcv/io/all"
               "clcv/ml/all"
               "clcv/graphics/all")
  :in-order-to ((test-op (load-op "clcv/test/all")))
  :perform (test-op (o c) (symbol-call :clcv/test/all :test-suite))
  :serial nil
  :components ((:file "clcv")
               (:static-file "README.md")
               (:static-file "LICENSE")
               (:static-file "TODO.org")))


;; ERROR with test
(defsystem "clcv/test" :depends-on ("clcv/test/all"))


(register-system-packages "clcv/graphic/all" '(:clcv-graphics))
(register-system-packages "clcv/matrix/all" '(:clcv-matrix))
(register-system-packages "clcv/core/all"   '(:clcv-core))
(register-system-packages "clcv/dip/all"    '(:clcv-dip))
(register-system-packages "clcv/gui/all"    '(:clcv-gui))
(register-system-packages "clcv/io/all"     '(:clcv-io))
(register-system-packages "clcv/ml/all"     '(:clcv-ml))
(register-system-packages "clcv/test/all"   '(:clcv-test))
