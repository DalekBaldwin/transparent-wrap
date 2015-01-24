(defpackage :transparent-wrap-test.original
  (:use :cl)
  (:export
   #:no-params
   #:required
   #:required-optional
   #:required-optional-rest
   #:required-key))

(in-package :transparent-wrap-test.original)

(defun no-params () nil)

(defun required (a b)
  (list a b))

(defun required-optional (a b &optional c (d :d-default) (e :e-default e-supplied))
  (list a b c d e e-supplied))

(defun required-optional-rest (a b &optional c (d :d-default) (e :e-default e-supplied) &rest f)
  (list a b c d e e-supplied f))

(defun required-key (a b &key c (d :d-default) (e :e-default e-supplied)
                           ((:z f)) ((:y g) :g-default) ((:x h) :h-default h-supplied))
  (list a b c d e e-supplied f g h h-supplied))
