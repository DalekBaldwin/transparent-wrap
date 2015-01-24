(in-package :cl-user)

(defpackage :transparent-wrap
  (:use :cl)
  (:export
   #:create-opaque-defun
   #:opaque-defun
   #:create-transparent-defun
   #:transparent-defun
   #:create-opaque-defmacro
   #:opaque-defmacro
   #:create-transparent-defmacro
   #:transparent-defmacro))
