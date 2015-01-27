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
   #:required-rest-key-aux
   #:optional
   #:optional-aux
   #:optional-rest
   #:optional-rest-aux
   #:optional-rest-key
   #:optional-rest-key-aux
   #:optional-key
   #:optional-key-aux
   #:key
   #:key-aux
   #:rest_
   #:rest-aux
   #:rest-key
   #:rest-key-aux))

(in-package :transparent-wrap-test.original)

(defparameter *state* nil)

(defun no-params () nil)

(defun no-params-aux (&aux a (b (push a *state*)))
  a b)

(defun required
    (a b)
  (values a b))

(defun required-aux
    (a b
     &aux c (d (push (list a b c) *state*)))
  (values a b c d))

(defun required-optional
    (a b
     &optional c (d (push :d-default *state*)) (e (push :e-default *state*) e-supplied))
  (values a b c d e e-supplied))

(defun required-optional-aux
    (a b
     &optional c (d (push :d-default *state*)) (e (push :e-default *state*) e-supplied)
     &aux f (g (push (list a b c d e e-supplied f) *state*)))
  (values a b c d e e-supplied f g))

(defun required-optional-rest
    (a b
     &optional c (d (push :d-default *state*)) (e (push :e-default *state*) e-supplied)
     &rest rest)
  (values a b c d e e-supplied rest))

(defun required-optional-rest-aux
    (a b
     &optional c (d (push :d-default *state*)) (e (push :e-default *state*) e-supplied)
     &rest rest
     &aux f (g (push (list a b c d e e-supplied rest f) *state*)))
  (values a b c d e e-supplied rest f g))

(defun required-optional-rest-key
    (a b
     &optional c (d (push :d-default *state*)) (e (push :e-default *state*) e-supplied)
     &rest rest
     &key f (g (push :g-default *state*)) (h (push :h-default *state*) h-supplied)
       ((:z i)) ((:y j) (push :j-default *state*))
       ((:x k) (push :k-default *state*) k-supplied))
  (values a b c d e e-supplied f g h h-supplied i j k k-supplied rest))

(defun required-optional-rest-key-aux
    (a b
     &optional c  (d (push :d-default *state*)) (e (push :e-default *state*) e-supplied)
     &rest rest
     &key f (g (push :g-default *state*)) (h (push :h-default *state*) h-supplied)
       ((:z i)) ((:y j) (push :j-default *state*))
       ((:x k) (push :k-default *state*) k-supplied)
     &aux l (m (push (list a b c d e e-supplied f g h
                           h-supplied i j k k-supplied rest l) *state*)))
  (values a b c d e e-supplied f g h h-supplied i j k k-supplied rest l m))

(defun required-optional-key
    (a b
     &optional c (d (push :d-default *state*)) (e (push :e-default *state*) e-supplied)
     &key f (g (push :g-default *state*)) (h (push :h-default *state*) h-supplied)
       ((:z i)) ((:y j) (push :j-default *state*))
       ((:x k) (push :k-default *state*) k-supplied))
  (values a b c d e e-supplied f g h h-supplied i j k k-supplied))

(defun required-optional-key-aux
    (a b
     &optional c (d (push :d-default *state*)) (e (push :e-default *state*) e-supplied)
     &key f (g (push :g-default *state*)) (h (push :h-default *state*) h-supplied)
       ((:z i)) ((:y j) (push :j-default *state*))
       ((:x k) (push :k-default *state*) k-supplied)
     &aux l (m (push (list a b c d e e-supplied f g h
                           h-supplied i j k k-supplied l) *state*)))
  (values a b c d e e-supplied f g h h-supplied i j k k-supplied l m))

(defun required-key
    (a b
     &key c (d (push :d-default *state*)) (e (push :e-default *state*) e-supplied)
       ((:z f)) ((:y g) (push :g-default *state*))
       ((:x h) (push :h-default *state*) h-supplied))
  (values a b c d e e-supplied f g h h-supplied))

(defun required-key-aux
    (a b
     &key c (d (push :d-default *state*)) (e (push :e-default *state*) e-supplied)
       ((:z f)) ((:y g) (push :g-default *state*))
       ((:x h) (push :h-default *state*) h-supplied)
     &aux i (j (push (list a b c d e e-supplied f g h h-supplied i) *state*)))
  (values a b c d e e-supplied f g h h-supplied i j))

(defun required-rest
    (a b
     &rest rest)
  (values a b rest))

(defun required-rest-aux
    (a b
     &rest rest
     &aux c (d (push (list a b rest c) *state*)))
  (values a b rest c d))

(defun required-rest-key
    (a b
     &rest rest
     &key c (d (push :d-default *state*)) (e (push :e-default *state*) e-supplied)
       ((:z f)) ((:y g) (push :g-default *state*))
       ((:x h) (push :h-default *state*) h-supplied))
  (values a b c d e e-supplied f g h h-supplied rest))

(defun required-rest-key-aux
    (a b
     &rest rest
     &key c (d (push :d-default *state*)) (e (push :e-default *state*) e-supplied)
       ((:z f)) ((:y g) (push :g-default *state*))
       ((:x h) (push :h-default *state*) h-supplied)
     &aux i (j (push (list a b c d e e-supplied f g h h-supplied rest i) *state*)))
  (values a b c d e e-supplied f g h h-supplied rest i j))

(defun optional
    (&optional a (b (push :b-default *state*)) (c (push :c-default *state*) c-supplied))
  (values a b c c-supplied))

(defun optional-aux
    (&optional a (b :b-default) (c :c-default c-supplied)
     &aux d (e (push (list a b c d) *state*)))
  (values a b c c-supplied d e))

(defun optional-rest
    (&optional a (b (push :b-default *state*)) (c (push :c-default *state*) c-supplied)
     &rest rest)
  (values a b c c-supplied rest))

(defun optional-rest-aux
    (&optional a (b (push :b-default *state*)) (c (push :c-default *state*) c-supplied)
     &rest rest
     &aux d (e (push (list a b c d rest) *state*)))
  (values a b c c-supplied rest d e))

(defun optional-rest-key
    (&optional a (b (push :b-default *state*)) (c (push :c-default *state*) c-supplied)
     &rest rest
     &key d (e (push :e-default *state*)) (f (push :f-default *state*) f-supplied)
       ((:z g)) ((:y h) (push :h-default *state*))
       ((:x i) (push :i-default *state*) i-supplied))
  (values a b c c-supplied d e f f-supplied g h i i-supplied rest))

(defun optional-rest-key-aux
    (&optional a (b (push :b-default *state*)) (c (push :c-default *state*) c-supplied)
     &rest rest
     &key d (e (push :e-default *state*)) (f (push :f-default *state*) f-supplied)
       ((:z g)) ((:y h) (push :h-default *state*))
       ((:x i) (push :i-default *state*) i-supplied)
     &aux j (k (push (list a b c c-supplied d e f
                           f-supplied g h i i-supplied rest j) *state*)))
  (values a b c c-supplied d e f f-supplied g h i i-supplied rest j k))

(defun optional-key
    (&optional a (b (push :b-default *state*)) (c (push :c-default *state*) c-supplied)
     &key d (e (push :e-default *state*)) (f (push :f-default *state*) f-supplied)
       ((:z g)) ((:y h) (push :h-default *state*))
       ((:x i) (push :i-default *state*) i-supplied))
  (values a b c c-supplied d e f f-supplied g h i i-supplied))

(defun optional-key-aux
    (&optional a (b (push :b-default *state*)) (c (push :c-default *state*) c-supplied)
     &key d (e (push :e-default *state*)) (f (push :f-default *state*) f-supplied)
       ((:z g)) ((:y h) (push :h-default *state*))
       ((:x i) (push :i-default *state*) i-supplied)
     &aux j (k (push (list a b c c-supplied d e f
                           f-supplied g h i i-supplied j) *state*)))
  (values a b c c-supplied d e f f-supplied g h i i-supplied j k))

(defun key
    (&key a (b (push :b-default *state*)) (c (push :c-default *state*) c-supplied)
       ((:z d)) ((:y e) (push :e-default *state*))
       ((:x f) (push :f-default *state*) f-supplied))
  (values a b c c-supplied d e f f-supplied))

(defun key-aux
    (&key a (b (push :b-default *state*)) (c (push :c-default *state*) c-supplied)
       ((:z d)) ((:y e) (push :e-default *state*))
       ((:x f) (push :f-default *state*) f-supplied)
     &aux g (h (push (list a b c c-supplied d e f f-supplied g) *state*)))
  (values a b c c-supplied d e f f-supplied g h))

(defun rest_
    (&rest rest)
  (values rest))

(defun rest-aux
    (&rest rest
     &aux a (b (push (list rest a) *state*)))
  (values rest a b))

(defun rest-key
    (&rest rest
     &key a (b (push :b-default *state*)) (c (push :c-default *state*) c-supplied)
       ((:z d)) ((:y e) (push :e-default *state*))
       ((:x f) (push :f-default *state*) f-supplied))
  (values a b c c-supplied d e f f-supplied rest))

(defun rest-key-aux
    (&rest rest
     &key a (b (push :b-default *state*)) (c (push :c-default *state*) c-supplied)
       ((:z d)) ((:y e) (push :e-default *state*))
       ((:x f) (push :f-default *state*) f-supplied)
     &aux g (h (push (list a b c c-supplied d e f f-supplied rest g) *state*)))
  (values a b c c-supplied d e f f-supplied rest g h))
