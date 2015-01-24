(defpackage :transparent-wrap-test.original
  (:use :cl)
  (:export
   #:no-params
   #:required
   #:required-optional
   #:required-optional-rest
   #:required-optional-rest-key
   #:required-optional-key
   #:required-key
   #:required-rest-key))

(in-package :transparent-wrap-test.original)

(defun no-params () nil)

(defun required (a b)
  (list a b))

(defun required-optional (a b &optional c (d :d-default) (e :e-default e-supplied))
  (list a b c d e e-supplied))

(defun required-optional-rest (a b &optional c (d :d-default) (e :e-default e-supplied) &rest f)
  (list a b c d e e-supplied f))

(defun required-optional-rest-key (a b &optional c (d :d-default) (e :e-default e-supplied)
                                   &rest rest
                                   &key f (g :g-default) (h :h-default h-supplied)
                                ((:z i)) ((:y j) :j-default) ((:x k) :k-default k-supplied))
  (list a b c d e e-supplied f g h h-supplied i j k k-supplied rest))

(defun required-optional-key (a b &optional c (d :d-default) (e :e-default e-supplied)
                              &key f (g :g-default) (h :h-default h-supplied)
                                ((:z i)) ((:y j) :j-default) ((:x k) :k-default k-supplied))
  (list a b c d e e-supplied f g h h-supplied i j k k-supplied))



(defun required-key (a b &key c (d :d-default) (e :e-default e-supplied)
                           ((:z f)) ((:y g) :g-default) ((:x h) :h-default h-supplied))
  (list a b c d e e-supplied f g h h-supplied))

(defun required-rest-key (a b &rest rest &key c (d :d-default) (e :e-default e-supplied)
                           ((:z f)) ((:y g) :g-default) ((:x h) :h-default h-supplied))
  (list a b c d e e-supplied f g h h-supplied rest))
