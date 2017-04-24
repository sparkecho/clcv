(uiop/package:define-package :clcv/core/type
  (:use :common-lisp)
  (:export :int8 :int16 :int32
           :uint8 :uint16 :uint32
           :single :double :char
           :logical))

(in-package :clcv/core/type)

(deftype :int8 () '(signed-byte 8))
(deftype :int16 () '(signed-byte 16))
(deftype :int32 () '(signed-byte 32))

(deftype :uint8 () '(unsigned-byte 8))
(deftype :uint16 () '(unsigned-byte 16))
(deftype :uint32 () '(unsigned-byte 32))

(deftype :single () 'single-float)
(deftype :double () 'double-float)

(deftype :char () 'character)
(deftype :logical () 'bit)
