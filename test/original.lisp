(defpackage :transparent-wrap-test.original
  (:use :cl)
  (:export
   #:no-params
   #:required
   #:required-optional
   #:required-optional-rest))

(in-package :transparent-wrap-test.original)

(defun no-params () nil)

(defun required (a b)
  (list a b))

(defun required-optional (a b &optional c (d :d-default) (e :e-default e-supplied))
  (list a b c d e e-supplied))

(defun required-optional-rest (a b &optional c (d :d-default) (e :e-default e-supplied) &rest f)
  (list a b c d e e-supplied f))
