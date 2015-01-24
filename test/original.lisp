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

(defun optional
    (&optional a (b :b-default) (c :c-default c-supplied))
  (list a b c c-supplied))

(defun optional-aux
    (&optional a (b :b-default) (c :c-default c-supplied)
     &aux (d (list a b c)))
  (list a b c c-supplied d))

(defun optional-rest
    (&optional a (b :b-default) (c :c-default c-supplied)
     &rest rest)
  (list a b c c-supplied rest))

(defun optional-rest-aux
    (&optional a (b :b-default) (c :c-default c-supplied)
     &rest rest
     &aux (d (list a b c rest)))
  (list a b c c-supplied rest d))

(defun optional-rest-key
    (&optional a (b :b-default) (c :c-default c-supplied)
     &rest rest
     &key d (e :e-default) (f :f-default f-supplied)
       ((:z g)) ((:y h) :h-default)
       ((:x i) :i-default i-supplied))
  (list a b c c-supplied rest d e f f-supplied g h i i-supplied))

(defun optional-rest-key-aux
    (&optional a (b :b-default) (c :c-default c-supplied)
     &rest rest
     &key d (e :e-default) (f :f-default f-supplied)
       ((:z g)) ((:y h) :h-default)
       ((:x i) :i-default i-supplied)
     &aux (j (list a b c c-supplied rest d e f f-supplied g h i i-supplied)))
  (list a b c c-supplied rest d e f f-supplied g h i i-supplied j))

(defun optional-key
    (&optional a (b :b-default) (c :c-default c-supplied)
     &key d (e :e-default) (f :f-default f-supplied)
       ((:z g)) ((:y h) :h-default)
       ((:x i) :i-default i-supplied))
  (list a b c c-supplied d e f f-supplied g h i i-supplied))

(defun optional-key-aux
    (&optional a (b :b-default) (c :c-default c-supplied)
     &key d (e :e-default) (f :f-default f-supplied)
       ((:z g)) ((:y h) :h-default)
       ((:x i) :i-default i-supplied)
     &aux (j (list a b c c-supplied d e f f-supplied g h i i-supplied)))
  (list a b c c-supplied d e f f-supplied g h i i-supplied j))

(defun key
    (&key a (b :b-default) (c :c-default c-supplied)
       ((:z d)) ((:y e) :e-default)
       ((:x f) :f-default f-supplied))
  (list a b c c-supplied d e f f-supplied))

(defun key-aux
    (&key a (b :b-default) (c :c-default c-supplied)
       ((:z d)) ((:y e) :e-default)
       ((:x f) :f-default f-supplied)
     &aux (g (list a b c c-supplied d e f f-supplied)))
  (list a b c c-supplied d e f f-supplied g))

(defun rest_
    (&rest rest)
  (list rest))

(defun rest-aux
    (&rest rest
     &aux (a (list rest)))
  (list rest a))

(defun rest-key
    (&rest rest
     &key a (b :b-default) (c :c-default c-supplied)
       ((:z d)) ((:y e) :e-default)
       ((:x f) :f-default f-supplied))
  (list a b c c-supplied d e f f-supplied rest))

(defun rest-key-aux
    (&rest rest
     &key a (b :b-default) (c :c-default c-supplied)
       ((:z d)) ((:y e) :e-default)
       ((:x f) :f-default f-supplied)
     &aux (g (list a b c c-supplied d e f f-supplied rest)))
  (list a b c c-supplied d e f f-supplied rest g))
