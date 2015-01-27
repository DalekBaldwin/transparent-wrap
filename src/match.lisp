(in-package :transparent-wrap)
(named-readtables:in-readtable :fare-quasiquote)

;;; ordinary lambda lists
;;; http://www.lispworks.com/documentation/HyperSpec/Body/03_da.htm
;;; ---------------------------------------------------------------
;;; lambda-list::= (var*
;;;                [&optional {var | (var [init-form [supplied-p-parameter]])}*]
;;;                [&rest var]
;;;                [&key {var | ({var | (keyword-name var)}
;;;                               [init-form [supplied-p-parameter]])}*
;;;                   [&allow-other-keys]]
;;;                [&aux {var | (var [init-form])}*])

;;; specialized lambda lists
;;; http://www.lispworks.com/documentation/HyperSpec/Body/03_dc.htm
;;; ---------------------------------------------------------------
;;; lambda-list::= ({var | (var [specializer])}*
;;;                 [&optional {var | (var [init-form [supplied-p-parameter]])}*]
;;;                 [&rest var]
;;;                 [&key {var | ({var | (keyword-name var)}
;;;                                [init-form [supplied-p-parameter]])}*
;;;                    [&allow-other-keys]]
;;;                 [&aux {var | (var [init-form])}*]) 

(defstruct required-param
  name)

(defstruct specialized-param
  whole
  name
  specializer)

(defstruct optional-param
  whole
  name
  init-form
  supplied-p-parameter)

(defstruct rest-param
  name)

(defstruct key-param
  whole
  name
  keyword-name
  init-form
  supplied-p-parameter)

(defstruct aux-param
  whole
  name
  init-form)

(defpattern ? (pattern)
  `(or nil ,pattern))

(defpattern ?* (&rest patterns)
  `(or nil
       (list* ,@patterns)))

(defun match-specialized (param)
  (match param
    ((or (list* name (?* specializer nil))
         name)
     (make-specialized-param
      :whole param
      :name name
      :specialized specializer))))

(defun match-optional (param)
  (match param
    ((or
      (list* name (?* init-form (?* supplied-p-parameter nil)))
      name)
     (make-optional-param
      :whole param
      :name name
      :init-form init-form
      :supplied-p-parameter supplied-p-parameter))))

(defun match-key (param)
  (match param
    ((or
      (list* (or (list keyword-name name)
                 name)
             (?* init-form (?* supplied-p-parameter nil)))
      name)
     (make-key-param
      :whole param
      :name name
      :keyword-name keyword-name
      :init-form init-form
      :supplied-p-parameter supplied-p-parameter))))

(defun match-aux (param)
  (match param
    ((or
      (list* name (?* init-form nil))
      name)
     (make-aux-param
      :whole param
      :name name
      :init-form init-form))))

(defun match-lambda-list (params)
  (match-requireds params))

;; todo: generate wrappers for every method or have a catch-all policy
(defun match-specialized-lambda-list (params)
  (match-specialized-requireds (params)))

(defun match-requireds (params)
  (match params
    (`(&optional ,@x)
      (match-optionals x))
    (`(&rest ,rest ,@x)
      (list* (make-rest-param :name rest) (match-post-rest x)))
    (`(&key ,@x)
      (match-keys x))
    (`(&aux ,@x)
      (match-auxes x))
    (`(,x ,@y)
      (list* (make-required-param :name x) (match-requireds y)))
    (nil
     nil)))

(defun match-specialized-requireds (params)
  (match params
    (`(&optional ,@x)
      (match-optionals x))
    (`(&rest ,rest ,@x)
      (list* (make-rest-param :name rest) (match-post-rest x)))
    (`(&key ,@x)
      (match-keys x))
    (`(&aux ,@x)
      (match-auxes x))
    (`(,x ,@y)
      (list* (match-specialized x) (match-specialized-requireds y)))
    (nil
     nil)))

(defun match-optionals (params)
  (match params
    (`(&rest ,rest ,@x)
      (list* (make-rest-param :name rest) (match-post-rest x)))
    (`(&key ,@x)
      (match-keys x))
    (`(&aux ,@x)
      (match-auxes x))
    (`(,x ,@y)
      (list* (match-optional x) (match-optionals y)))
    (nil
     nil)))

(defun match-post-rest (params)
  (match params
    (`(&key ,@x)
      (match-keys x))
    (`(&aux ,@x)
      (match-auxes x))
    (nil
     nil)))

(defun match-keys (params)
  (match params
    (`(&aux ,@x)
      (match-auxes x))
    (`(&allow-other-keys ,@x)
      (list* '&allow-other-keys (match-post-keys x)))
    (`(,x ,@y)
      (list* (match-key x) (match-keys y)))
    (nil
     nil)))

(defun match-post-keys (params)
  (match params
    (`(&aux ,@x)
      (match-auxes x))
    (nil
     nil)))

(defun match-auxes (params)
  (match params
    (`(,x ,@y)
      (list* (match-aux x) (match-auxes y)))
    (nil
     nil)))
