(defpackage :transparent-wrap-test.stateful
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
   
   #:generic-key
   #:generic-rest
   #:generic-rest-key
   ))

(in-package :transparent-wrap-test.stateful)

(defparameter *state* nil)

(defun no-params ()
  (push :body *state*)
  *state*)

(defun no-params-aux (&aux a (b (push a *state*)))
  (push :body *state*)
  (values a b *state*))

(defun required
    (a b)
  (push :body *state*)
  (values a b *state*))

(defun required-aux
    (a b
     &aux c (d (push (list a b c) *state*)))
  (push :body *state*)
  (values a b c d *state*))

(defun required-optional
    (a b
     &optional c (d (push :d-default *state*)) (e (push :e-default *state*) e-supplied))
  (push :body *state*)
  (values a b c d e e-supplied *state*))

(defun required-optional-aux
    (a b
     &optional c (d (push :d-default *state*)) (e (push :e-default *state*) e-supplied)
     &aux f (g (push (list a b c d e e-supplied f) *state*)))
  (push :body *state*)
  (values a b c d e e-supplied f g *state*))

(defun required-optional-rest
    (a b
     &optional c (d (push :d-default *state*)) (e (push :e-default *state*) e-supplied)
     &rest rest)
  (push :body *state*)
  (values a b c d e e-supplied rest *state*))

(defun required-optional-rest-aux
    (a b
     &optional c (d (push :d-default *state*)) (e (push :e-default *state*) e-supplied)
     &rest rest
     &aux f (g (push (list a b c d e e-supplied rest f) *state*)))
  (push :body *state*)
  (values a b c d e e-supplied rest f g *state*))

(defun required-optional-rest-key
    (a b
     &optional c (d (push :d-default *state*)) (e (push :e-default *state*) e-supplied)
     &rest rest
     &key f (g (push :g-default *state*)) (h (push :h-default *state*) h-supplied)
       ((:z i)) ((:y j) (push :j-default *state*))
       ((:x k) (push :k-default *state*) k-supplied))
  (push :body *state*)
  (values a b c d e e-supplied f g h h-supplied i j k k-supplied rest *state*))

(defun required-optional-rest-key-aux
    (a b
     &optional c  (d (push :d-default *state*)) (e (push :e-default *state*) e-supplied)
     &rest rest
     &key f (g (push :g-default *state*)) (h (push :h-default *state*) h-supplied)
       ((:z i)) ((:y j) (push :j-default *state*))
       ((:x k) (push :k-default *state*) k-supplied)
     &aux l (m (push (list a b c d e e-supplied f g h
                           h-supplied i j k k-supplied rest l) *state*)))
  (push :body *state*)
  (values a b c d e e-supplied f g h h-supplied i j k k-supplied rest l m *state*))

(defun required-optional-key
    (a b
     &optional c (d (push :d-default *state*)) (e (push :e-default *state*) e-supplied)
     &key f (g (push :g-default *state*)) (h (push :h-default *state*) h-supplied)
       ((:z i)) ((:y j) (push :j-default *state*))
       ((:x k) (push :k-default *state*) k-supplied))
  (push :body *state*)
  (values a b c d e e-supplied f g h h-supplied i j k k-supplied *state*))

(defun required-optional-key-aux
    (a b
     &optional c (d (push :d-default *state*)) (e (push :e-default *state*) e-supplied)
     &key f (g (push :g-default *state*)) (h (push :h-default *state*) h-supplied)
       ((:z i)) ((:y j) (push :j-default *state*))
       ((:x k) (push :k-default *state*) k-supplied)
     &aux l (m (push (list a b c d e e-supplied f g h
                           h-supplied i j k k-supplied l) *state*)))
  (push :body *state*)
  (values a b c d e e-supplied f g h h-supplied i j k k-supplied l m *state*))

(defun required-key
    (a b
     &key c (d (push :d-default *state*)) (e (push :e-default *state*) e-supplied)
       ((:z f)) ((:y g) (push :g-default *state*))
       ((:x h) (push :h-default *state*) h-supplied))
  (push :body *state*)
  (values a b c d e e-supplied f g h h-supplied *state*))

(defun required-key-aux
    (a b
     &key c (d (push :d-default *state*)) (e (push :e-default *state*) e-supplied)
       ((:z f)) ((:y g) (push :g-default *state*))
       ((:x h) (push :h-default *state*) h-supplied)
     &aux i (j (push (list a b c d e e-supplied f g h h-supplied i) *state*)))
  (push :body *state*)
  (values a b c d e e-supplied f g h h-supplied i j *state*))

(defun required-rest
    (a b
     &rest rest)
  (push :body *state*)
  (values a b rest *state*))

(defun required-rest-aux
    (a b
     &rest rest
     &aux c (d (push (list a b rest c) *state*)))
  (push :body *state*)
  (values a b rest c d *state*))

(defun required-rest-key
    (a b
     &rest rest
     &key c (d (push :d-default *state*)) (e (push :e-default *state*) e-supplied)
       ((:z f)) ((:y g) (push :g-default *state*))
       ((:x h) (push :h-default *state*) h-supplied))
  (push :body *state*)
  (values a b c d e e-supplied f g h h-supplied rest *state*))

(defun required-rest-key-aux
    (a b
     &rest rest
     &key c (d (push :d-default *state*)) (e (push :e-default *state*) e-supplied)
       ((:z f)) ((:y g) (push :g-default *state*))
       ((:x h) (push :h-default *state*) h-supplied)
     &aux i (j (push (list a b c d e e-supplied f g h h-supplied rest i) *state*)))
  (push :body *state*)
  (values a b c d e e-supplied f g h h-supplied rest i j *state*))

(defun optional
    (&optional a (b (push :b-default *state*)) (c (push :c-default *state*) c-supplied))
  (push :body *state*)
  (values a b c c-supplied *state*))

(defun optional-aux
    (&optional a (b :b-default) (c :c-default c-supplied)
     &aux d (e (push (list a b c d) *state*)))
  (push :body *state*)
  (values a b c c-supplied d e *state*))

(defun optional-rest
    (&optional a (b (push :b-default *state*)) (c (push :c-default *state*) c-supplied)
     &rest rest)
  (push :body *state*)
  (values a b c c-supplied rest *state*))

(defun optional-rest-aux
    (&optional a (b (push :b-default *state*)) (c (push :c-default *state*) c-supplied)
     &rest rest
     &aux d (e (push (list a b c d rest) *state*)))
  (push :body *state*)
  (values a b c c-supplied rest d e *state*))

(defun optional-rest-key
    (&optional a (b (push :b-default *state*)) (c (push :c-default *state*) c-supplied)
     &rest rest
     &key d (e (push :e-default *state*)) (f (push :f-default *state*) f-supplied)
       ((:z g)) ((:y h) (push :h-default *state*))
       ((:x i) (push :i-default *state*) i-supplied))
  (push :body *state*)
  (values a b c c-supplied d e f f-supplied g h i i-supplied rest *state*))

(defun optional-rest-key-aux
    (&optional a (b (push :b-default *state*)) (c (push :c-default *state*) c-supplied)
     &rest rest
     &key d (e (push :e-default *state*)) (f (push :f-default *state*) f-supplied)
       ((:z g)) ((:y h) (push :h-default *state*))
       ((:x i) (push :i-default *state*) i-supplied)
     &aux j (k (push (list a b c c-supplied d e f
                           f-supplied g h i i-supplied rest j) *state*)))
  (push :body *state*)
  (values a b c c-supplied d e f f-supplied g h i i-supplied rest j k *state*))

(defun optional-key
    (&optional a (b (push :b-default *state*)) (c (push :c-default *state*) c-supplied)
     &key d (e (push :e-default *state*)) (f (push :f-default *state*) f-supplied)
       ((:z g)) ((:y h) (push :h-default *state*))
       ((:x i) (push :i-default *state*) i-supplied))
  (push :body *state*)
  (values a b c c-supplied d e f f-supplied g h i i-supplied *state*))

(defun optional-key-aux
    (&optional a (b (push :b-default *state*)) (c (push :c-default *state*) c-supplied)
     &key d (e (push :e-default *state*)) (f (push :f-default *state*) f-supplied)
       ((:z g)) ((:y h) (push :h-default *state*))
       ((:x i) (push :i-default *state*) i-supplied)
     &aux j (k (push (list a b c c-supplied d e f
                           f-supplied g h i i-supplied j) *state*)))
  (push :body *state*)
  (values a b c c-supplied d e f f-supplied g h i i-supplied j k *state*))

(defun key
    (&key a (b (push :b-default *state*)) (c (push :c-default *state*) c-supplied)
       ((:z d)) ((:y e) (push :e-default *state*))
       ((:x f) (push :f-default *state*) f-supplied))
  (push :body *state*)
  (values a b c c-supplied d e f f-supplied *state*))

(defun key-aux
    (&key a (b (push :b-default *state*)) (c (push :c-default *state*) c-supplied)
       ((:z d)) ((:y e) (push :e-default *state*))
       ((:x f) (push :f-default *state*) f-supplied)
     &aux g (h (push (list a b c c-supplied d e f f-supplied g) *state*)))
  (push :body *state*)
  (values a b c c-supplied d e f f-supplied g h *state*))

(defun rest_
    (&rest rest)
  (push :body *state*)
  (values rest *state*))

(defun rest-aux
    (&rest rest
     &aux a (b (push (list rest a) *state*)))
  (push :body *state*)
  (values rest a b *state*))

(defun rest-key
    (&rest rest
     &key a (b (push :b-default *state*)) (c (push :c-default *state*) c-supplied)
       ((:z d)) ((:y e) (push :e-default *state*))
       ((:x f) (push :f-default *state*) f-supplied))
  (push :body *state*)
  (values a b c c-supplied d e f f-supplied rest *state*))

(defun rest-key-aux
    (&rest rest
     &key a (b (push :b-default *state*)) (c (push :c-default *state*) c-supplied)
       ((:z d)) ((:y e) (push :e-default *state*))
       ((:x f) (push :f-default *state*) f-supplied)
     &aux g (h (push (list a b c c-supplied d e f f-supplied rest g) *state*)))
  (push :body *state*)
  (values a b c c-supplied d e f f-supplied rest g h *state*))

(defun alternating-optional-supplied
    (&optional
       (a nil a-supplied)
       b
       (c nil c-supplied))
  (values a a-supplied b c c-supplied))

#.
`(progn
   ,@(let ((combinations)
           (arguments '(a b)))
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
                                    :transparent-wrap-test.stateful)
                                  (&optional
                                     (a
                                      ,(if (member 'a init-forms)
                                           `(push :a-init *state*)
                                           nil)
                                      ,@(when (member 'a supplieds)
                                              `(a-supplied)))
                                     (b
                                      ,(if (member 'b init-forms)
                                           `(push :b-init *state*)
                                           nil)
                                      ,@(when (member 'b supplieds)
                                              `(b-supplied))))
                                (values a ,@(when (member 'a supplieds)
                                                  `(a-supplied))
                                        b ,@(when (member 'b supplieds)
                                                  `(b-supplied))
                                        *state*))
                             combinations))
                          arguments :length j)))
                 arguments :length i))
          combinations))

(defgeneric generic-key (a &optional b &key c))

(defmethod generic-key ((a integer) &optional (b (push :b-default-int *state*))
                        &key (c (push :c-default-int *state*)))
  (values a b c *state*))

(defmethod generic-key ((a cons) &optional (b (push :b-default-cons *state*))
                        &key (c (push :c-default-cons *state*)))
  (values a b c *state*))

(defgeneric generic-rest (a &optional b &rest rest))

(defmethod generic-rest ((a integer) &optional (b (push :b-default-int *state*))
                         &key (c (push :c-default-int *state*)))
  (values a b c *state*))

(defmethod generic-rest ((a cons) &optional (b (push :b-default-cons *state*))
                         &key (c (push :c-default-cons *state*)))
  (values a b c *state*))

(defgeneric generic-rest-key (a &optional b &rest rest &key c))

(defmethod generic-rest-key ((a integer) &optional (b (push :b-default-int *state*))
                             &key (c (push :c-default-int *state*)))
  (values a b c *state*))

(defmethod generic-rest-key ((a cons) &optional (b (push :b-default-cons *state*))
                             &key (c (push :c-default-cons *state*)))
  (values a b c *state*))
