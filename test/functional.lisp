(defpackage :transparent-wrap-test.functional
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
   #:rest-key-aux
   
   #:alternating-optional-supplied
   ))

(in-package :transparent-wrap-test.functional)

(defun no-params ()
  (list nil))

(defun no-params-aux (&aux a (b a))
  (values a b))

(defun required
    (a b)
  (values a b))

(defun required-aux
    (a b
     &aux c (d (list a b c)))
  (values a b c d))

(defun required-optional
    (a b
     &optional c (d :d-default) (e :e-default e-supplied))
  (values a b c d e e-supplied))

(defun required-optional-aux
    (a b
     &optional c (d :d-default) (e :e-default e-supplied)
     &aux f (g (list a b c d e e-supplied f)))
  (values a b c d e e-supplied f g))

(defun required-optional-rest
    (a b
     &optional c (d :d-default) (e :e-default e-supplied)
     &rest rest)
  (values a b c d e e-supplied rest))

(defun required-optional-rest-aux
    (a b
     &optional c (d :d-default) (e :e-default e-supplied)
     &rest rest
     &aux f (g (list a b c d e e-supplied rest f)))
  (values a b c d e e-supplied rest f g))

(defun required-optional-rest-key
    (a b
     &optional c (d :d-default) (e :e-default e-supplied)
     &rest rest
     &key f (g :g-default) (h :h-default h-supplied)
       ((:z i)) ((:y j) :j-default)
       ((:x k) :k-default k-supplied))
  (values a b c d e e-supplied f g h h-supplied i j k k-supplied rest))

(defun required-optional-rest-key-aux
    (a b
     &optional c  (d :d-default) (e :e-default e-supplied)
     &rest rest
     &key f (g :g-default) (h :h-default h-supplied)
       ((:z i)) ((:y j) :j-default)
       ((:x k) :k-default k-supplied)
     &aux l (m (list a b c d e e-supplied f g h
                     h-supplied i j k k-supplied rest l)))
  (values a b c d e e-supplied f g h h-supplied i j k k-supplied rest l m))

(defun required-optional-key
    (a b
     &optional c (d :d-default) (e :e-default e-supplied)
     &key f (g :g-default) (h :h-default h-supplied)
       ((:z i)) ((:y j) :j-default)
       ((:x k) :k-default k-supplied))
  (values a b c d e e-supplied f g h h-supplied i j k k-supplied))

(defun required-optional-key-aux
    (a b
     &optional c (d :d-default) (e :e-default e-supplied)
     &key f (g :g-default) (h :h-default h-supplied)
       ((:z i)) ((:y j) :j-default)
       ((:x k) :k-default k-supplied)
     &aux l (m (list a b c d e e-supplied f g h
                     h-supplied i j k k-supplied l)))
  (values a b c d e e-supplied f g h h-supplied i j k k-supplied l m))

(defun required-key
    (a b
     &key c (d :d-default) (e :e-default e-supplied)
       ((:z f)) ((:y g) :g-default)
       ((:x h) :h-default h-supplied))
  (values a b c d e e-supplied f g h h-supplied))

(defun required-key-aux
    (a b
     &key c (d :d-default) (e :e-default e-supplied)
       ((:z f)) ((:y g) :g-default)
       ((:x h) :h-default h-supplied)
     &aux i (j (list a b c d e e-supplied f g h h-supplied i)))
  (values a b c d e e-supplied f g h h-supplied i j))

(defun required-rest
    (a b
     &rest rest)
  (values a b rest))

(defun required-rest-aux
    (a b
     &rest rest
     &aux c (d (list a b rest c)))
  (values a b rest c d))

(defun required-rest-key
    (a b
     &rest rest
     &key c (d :d-default) (e :e-default e-supplied)
       ((:z f)) ((:y g) :g-default)
       ((:x h) :h-default h-supplied))
  (values a b c d e e-supplied f g h h-supplied rest))

(defun required-rest-key-aux
    (a b
     &rest rest
     &key c (d :d-default) (e :e-default e-supplied)
       ((:z f)) ((:y g) :g-default)
       ((:x h) :h-default h-supplied)
     &aux i (j (list a b c d e e-supplied f g h h-supplied rest i)))
  (values a b c d e e-supplied f g h h-supplied rest i j))

(defun optional
    (&optional a (b :b-default) (c :c-default c-supplied))
  (values a b c c-supplied))

(defun optional-aux
    (&optional a (b :b-default) (c :c-default c-supplied)
     &aux d (e (list a b c d)))
  (values a b c c-supplied d e))

(defun optional-rest
    (&optional a (b :b-default) (c :c-default c-supplied)
     &rest rest)
  (values a b c c-supplied rest))

(defun optional-rest-aux
    (&optional a (b :b-default) (c :c-default c-supplied)
     &rest rest
     &aux d (e (list a b c d rest)))
  (values a b c c-supplied rest d e))

(defun optional-rest-key
    (&optional a (b :b-default) (c :c-default c-supplied)
     &rest rest
     &key d (e :e-default) (f :f-default f-supplied)
       ((:z g)) ((:y h) :h-default)
       ((:x i) :i-default i-supplied))
  (values a b c c-supplied d e f f-supplied g h i i-supplied rest))

(defun optional-rest-key-aux
    (&optional a (b :b-default) (c :c-default c-supplied)
     &rest rest
     &key d (e :e-default) (f :f-default f-supplied)
       ((:z g)) ((:y h) :h-default)
       ((:x i) :i-default i-supplied)
     &aux j (k (list a b c c-supplied d e f
                     f-supplied g h i i-supplied rest j)))
  (values a b c c-supplied d e f f-supplied g h i i-supplied rest j k))

(defun optional-key
    (&optional a (b :b-default) (c :c-default c-supplied)
     &key d (e :e-default) (f :f-default f-supplied)
       ((:z g)) ((:y h) :h-default)
       ((:x i) :i-default i-supplied))
  (values a b c c-supplied d e f f-supplied g h i i-supplied))

(defun optional-key-aux
    (&optional a (b :b-default) (c :c-default c-supplied)
     &key d (e :e-default) (f :f-default f-supplied)
       ((:z g)) ((:y h) :h-default)
       ((:x i) :i-default i-supplied)
     &aux j (k (list a b c c-supplied d e f
                     f-supplied g h i i-supplied j)))
  (values a b c c-supplied d e f f-supplied g h i i-supplied j k))

(defun key
    (&key a (b :b-default) (c :c-default c-supplied)
       ((:z d)) ((:y e) :e-default)
       ((:x f) :f-default f-supplied))
  (values a b c c-supplied d e f f-supplied))

(defun key-aux
    (&key a (b :b-default) (c :c-default c-supplied)
       ((:z d)) ((:y e) :e-default)
       ((:x f) :f-default f-supplied)
     &aux g (h (list a b c c-supplied d e f f-supplied g)))
  (values a b c c-supplied d e f f-supplied g h))

(defun rest_
    (&rest rest)
  (values rest))

(defun rest-aux
    (&rest rest
     &aux a (b (list rest a)))
  (values rest a b))

(defun rest-key
    (&rest rest
     &key a (b :b-default) (c :c-default c-supplied)
       ((:z d)) ((:y e) :e-default)
       ((:x f) :f-default f-supplied))
  (values a b c c-supplied d e f f-supplied rest))

(defun rest-key-aux
    (&rest rest
     &key a (b :b-default) (c :c-default c-supplied)
       ((:z d)) ((:y e) :e-default)
       ((:x f) :f-default f-supplied)
     &aux g (h (list a b c c-supplied d e f f-supplied rest g)))
  (values a b c c-supplied d e f f-supplied rest g h))

(defun alternating-optional-supplied
    (&optional
       (a nil a-supplied)
       b
       (c nil c-supplied))
  (values a a-supplied b c c-supplied))

#.
`(progn
   ,@(let ((combinations)
           (arguments '(a b c d)))
          (loop for i from 0 to (length arguments)
             do (alexandria:map-combinations
                 (lambda (init-forms)
                   (loop for j from 0 to (length arguments)
                      do (alexandria:map-combinations
                          (lambda (supplieds)
                            (push
                             `(defun
                                  ,(intern
                                    (concatenate
                                     'string
                                     "OPTIONAL-MATRIX-I"
                                     (apply #'concatenate 'string
                                            (loop for arg in init-forms
                                               collect (princ-to-string arg)))
                                     "-S"
                                     (apply #'concatenate 'string
                                            (loop for arg in supplieds
                                               collect (princ-to-string arg))))
                                    :transparent-wrap-test.functional)
                                  (&optional
                                     (a
                                      ,(if (member 'a init-forms)
                                           :a-init
                                           nil)
                                      ,@(when (member 'a supplieds)
                                              `(a-supplied)))
                                     (b
                                      ,(if (member 'b init-forms)
                                           :b-init
                                           nil)
                                      ,@(when (member 'b supplieds)
                                              `(b-supplied)))
                                     (c
                                      ,(if (member 'c init-forms)
                                           :c-init
                                           nil)
                                      ,@(when (member 'c supplieds)
                                              `(c-supplied)))
                                     (d
                                      ,(if (member 'd init-forms)
                                           :d-init
                                           nil)
                                      ,@(when (member 'd supplieds)
                                              `(d-supplied))))
                                (values a ,@(when (member 'a supplieds)
                                                  `(a-supplied))
                                        b ,@(when (member 'b supplieds)
                                                  `(b-supplied))
                                        c ,@(when (member 'c supplieds)
                                                  `(c-supplied))
                                        d ,@(when (member 'd supplieds)
                                                  `(d-supplied))))
                             combinations))
                          arguments :length j)))
                 arguments :length i))
          combinations))
