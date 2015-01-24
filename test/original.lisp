(defpackage :transparent-wrap-test.original
  (:use :cl)
  (:export
   #:no-params
   #:no-params-aux
   #:required
   #:required-aux
   #:required-optional
   #:required-optional-aux
   #:required-optional-rest
   #:required-optional-rest-aux
   #:required-optional-rest-key
   #:required-optional-rest-key-aux
   #:required-optional-key
   #:required-optional-key-aux
   #:required-key
   #:required-key-aux
   #:required-rest
   #:required-rest-aux
   #:required-rest-key
   #:required-rest-key-aux))

(in-package :transparent-wrap-test.original)

(defun no-params () nil)

(defun no-params-aux (&aux a)
  a)

(defun required
    (a b)
  (list a b))

(defun required-aux
    (a b
     &aux (c (list a b)))
  (list a b c))

(defun required-optional
    (a b
     &optional c (d :d-default) (e :e-default e-supplied))
  (list a b c d e e-supplied))

(defun required-optional-aux
    (a b
     &optional c (d :d-default) (e :e-default e-supplied)
     &aux (f (list a b c d e e-supplied)))
  (list a b c d e e-supplied f))

(defun required-optional-rest
    (a b
     &optional c (d :d-default) (e :e-default e-supplied)
     &rest f)
  (list a b c d e e-supplied f))

(defun required-optional-rest-aux
    (a b
     &optional c (d :d-default) (e :e-default e-supplied)
     &rest f
     &aux (g (list a b c d e e-supplied f)))
  (list a b c d e e-supplied f g))

(defun required-optional-rest-key
    (a b
     &optional c (d :d-default) (e :e-default e-supplied)
     &rest rest
     &key f (g :g-default) (h :h-default h-supplied)
       ((:z i)) ((:y j) :j-default)
       ((:x k) :k-default k-supplied))
  (list a b c d e e-supplied f g h h-supplied i j k k-supplied rest))

(defun required-optional-rest-key-aux
    (a b
     &optional c (d :d-default) (e :e-default e-supplied)
     &rest rest
     &key f (g :g-default) (h :h-default h-supplied)
       ((:z i)) ((:y j) :j-default)
       ((:x k) :k-default k-supplied)
     &aux (l (list a b c d e e-supplied f g h
                   h-supplied i j k k-supplied rest)))
  (list a b c d e e-supplied f g h h-supplied i j k k-supplied rest l))

(defun required-optional-key
    (a b
     &optional c (d :d-default) (e :e-default e-supplied)
     &key f (g :g-default) (h :h-default h-supplied)
       ((:z i)) ((:y j) :j-default) ((:x k) :k-default k-supplied))
  (list a b c d e e-supplied f g h h-supplied i j k k-supplied))

(defun required-optional-key-aux
    (a b
     &optional c (d :d-default) (e :e-default e-supplied)
     &key f (g :g-default) (h :h-default h-supplied)
       ((:z i)) ((:y j) :j-default) ((:x k) :k-default k-supplied)
     &aux (l (list a b c d e e-supplied f g h
                   h-supplied i j k k-supplied)))
  (list a b c d e e-supplied f g h h-supplied i j k k-supplied l))

(defun required-key
    (a b
     &key c (d :d-default) (e :e-default e-supplied)
       ((:z f)) ((:y g) :g-default) ((:x h) :h-default h-supplied))
  (list a b c d e e-supplied f g h h-supplied))

(defun required-key-aux
    (a b
     &key c (d :d-default) (e :e-default e-supplied)
       ((:z f)) ((:y g) :g-default) ((:x h) :h-default h-supplied)
     &aux (i (list a b c d e e-supplied f g h h-supplied)))
  (list a b c d e e-supplied f g h h-supplied i))

(defun required-rest
    (a b
     &rest rest)
  (list a b rest))

(defun required-rest-aux
    (a b
     &rest rest
     &aux (c (list a b rest)))
  (list a b rest c))

(defun required-rest-key
    (a b
     &rest rest
     &key c (d :d-default) (e :e-default e-supplied)
       ((:z f)) ((:y g) :g-default) ((:x h) :h-default h-supplied))
  (list a b c d e e-supplied f g h h-supplied rest))

(defun required-rest-key-aux
    (a b
     &rest rest
     &key c (d :d-default) (e :e-default e-supplied)
       ((:z f)) ((:y g) :g-default) ((:x h) :h-default h-supplied)
     &aux (i (list a b c d e e-supplied f g h h-supplied rest)))
  (list a b c d e e-supplied f g h h-supplied rest i))
