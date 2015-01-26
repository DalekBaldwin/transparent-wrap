(in-package :transparent-wrap)

(defun organize-arguments (arglist)
  (let ((directives (list '&optional '&rest '&key '&allow-other-keys '&aux))
        (collecting-directives (list :required '&optional '&rest '&key '&aux))
        (current-directive :required)
        (collected-args
         (make-hash-table)))
    (loop for directive in collecting-directives
       do (setf (gethash directive collected-args) nil))
    (loop for arg in arglist
       if (member arg directives)
       do (setf current-directive arg)
       else
       do (pushnew arg (gethash current-directive collected-args)))
    (apply #'values (mapcar
                     (lambda (directive)
                       (reverse (gethash directive collected-args)))
                     collecting-directives))))

#+nil
"lambda-list::= (var* 
                [&optional {var | (var [init-form [supplied-p-parameter]])}*] 
                [&rest var] 
                [&key {var | ({var | (keyword-name var)} [init-form [supplied-p-parameter]])}* [&allow-other-keys]] 
                [&aux {var | (var [init-form])}*]) "

(defparameter *lambda-list-grammar*
  '((required-var name)
    (optional-var (or name
                   (list name (? init-form (? supplied-p-parameter)))))
    (rest-var name)
    (keyword-var (or name
                  (list (or name
                              (list keyword-name name))
                         (? init-form (? supplied-p-parameter)))))
    (aux-var (or name
              (list (name (? init-form)))))
    (required (* required-var))
    (optional (? &optional
               (* optional-var)))
    (rest (? &rest rest-var))
    (key (? &key
          (* keyword-var)
          (? &allow-other-keys)))
    (aux (? &aux
          (* aux-var)))
    (lambda-list (required
                  optional
                  rest
                  key
                  aux))))

#+nil
(format t "~&~S~%"
        (match-lambda-list '(a b
     &optional c (d :d-default) (e :e-default e-supplied)
     &rest rest
     &key f (g :g-default) (h :h-default h-supplied)
       ((:z i)) ((:y j) :j-default)
       ((:x k) :k-default k-supplied)
     &aux (l (list a b c d e e-supplied f g h
                   h-supplied i j k k-supplied rest)))))
#+nil
(optima::pattern-expand-all '(or 
                              (list* name (?* init-form (?* supplied-p-parameter)))
                              name))
#+nil
(format t "~&~S~%"
        (match-keys '(f (g :g-default) (h :h-default h-supplied)
                      ((:z i)) ((:y j) :j-default)
                      ((:x k) :k-default k-supplied) &allow-other-keys)))





#+nil
(match-required '(&optional nerp))

#+nil
(match '(herp derp &optional barf)
  ((list a b c d)
    (list a b c d)))
#+nil
(match '(1 2 3 4)
  (`(1 ,@x ,y) (list x y)))

(defun create-keyword-params (key)
  (remove-duplicates
   (loop for k in key
      collect
        (if (atom k) k (first k)))
   :test (lambda (x y) (eql (or (and (atom x) x)
                                (first x))
                            (or (and (atom y) y)
                                (first y))))))

(defun create-optional-params (optional)
  (remove-duplicates
   (loop for o in optional
      collect
        (if (atom o) o (first o)))))

(defun create-keyword-args (key)
  (apply #'append
         (remove-duplicates
          (loop for k in key
             collect
               (cond
                 ((atom k)
                  (list
                   (intern (symbol-name k) :keyword)
                   k))
                 ((atom (first k))
                  (list
                   (intern (symbol-name (first k)) :keyword)
                   (first k)))
                 ((listp (first k))
                  (list
                   (first (first k))
                   (second (first k))))))
          :test (lambda (x y) (eql (first x) (first y))))))

(defun count-until-false (list)
  (or
   (loop for item in list
      while item
      count item into total
      finally (return total))
   0))

;; Function wrappers should look like this:
;; (lambda (function-call) `(wrap-something-around ,function-call))

(defun create-opaque-defun (function wrapper wrapping-package
                            &key alt-name)
  `(defun ,(or alt-name
               (intern (princ-to-string function) wrapping-package))
       (&rest args)
     ,(funcall wrapper
               `(apply (symbol-function ',function) args))))

;; Function-wrapping macros should look like normal macros:
;; (defmacro wrapper (wrapped-function-form)
;;   `(wrap-something-around ,wrapped-function-form))

(defmacro opaque-defun (function wrapper wrapping-package
                        &key alt-name)
  `(defun ,(or alt-name
               (intern (princ-to-string function) wrapping-package))
       (&rest args)
     (,wrapper (apply (symbol-function ',function) args))))

(defun create-body (function required optional->supplied rest key->supplied)
  (let ((non-optional-portion
         (cond
           (rest
            `(apply (symbol-function ',function)
                    ,@required
                    ,@(mapcar #'car optional->supplied)
                    ,@rest))
           (key->supplied
            `(apply (symbol-function ',function)
                    ,@required
                    ,@(mapcar #'car optional->supplied)
                    (let ((actual-keys))
                      (loop
                         for supplied in
                           (list ,@(loop for key in key->supplied
                                      collect (cdr key)))
                         for arg in
                           (list ,@(loop for key in key->supplied
                                      collect
                                        (let ((thing (car key)))
                                          (if (atom thing)
                                              thing
                                              (second thing)))))
                         for key in
                           (list ,@(loop for key in key->supplied
                                      collect
                                        (let ((thing (car key)))
                                          (if (atom thing)
                                              (intern
                                               (symbol-name thing)
                                               :keyword)
                                              (first thing)))))
                         when supplied
                         do
                           (push arg actual-keys)
                           (push key actual-keys))
                      actual-keys)))
           (t `(,function
                ,@required
                ,@(mapcar #'car optional->supplied))))))
    (if (null optional->supplied)
        non-optional-portion
        `(case (count-until-false
                (list ,@(loop for optional in optional->supplied
                           collect (cdr optional))))
           ,@(loop for optional in optional->supplied
                counting optional into i
                collect
                  `(,(1- i)
                     (,function
                      ,@required
                      ,@(mapcar
                         #'car
                         (subseq optional->supplied 0 (1- i)))))
                into cases
                finally
                  (return
                    (append
                     cases
                     (list
                      `(otherwise
                        ,non-optional-portion)))))))))



(defun create-transparent-defun% (function wrapper wrapping-package
                                  &key force-rest alt-name body-maker)
  (let ((arglist (trivial-arguments:arglist (symbol-function function))))
    
    ;; fall-through: give up on transparency
    (when (eql arglist :unknown)
      (return-from create-transparent-defun%
        (create-opaque-defun function wrapper wrapping-package
                            :alt-name alt-name)))
    


    (let* ((parsed (match-lambda-list arglist))
           (required (remove-if-not #'required-param-p parsed))
           (&optional (remove-if-not #'optional-param-p parsed))
           (&rest (remove-if-not #'rest-param-p parsed))
           (&key (remove-if-not #'key-param-p parsed))
           (&allow-other-keys (find '&allow-other-keys parsed))
           (&aux (remove-if-not #'aux-param-p parsed)))
      (declare (ignore &aux))
      (loop for key in &key
           (with-slots (init-form
                        #+ccl name) key
             ;; CCL returns keyword symbol and no other info
             #+ccl (setf name
                         (intern (symbol-name name) wrapping-package))
             (setf init-form nil)))
      (loop for optional in &optional
           (with-slots (init-form) optional
             (setf init-form nil)))
      (when (and (null &rest)
                 (or force-rest
                     ;; MUST pass possibly unknown args through
                     &allow-other-keys))
        (setf &rest (list (make-rest-param :name (gensym "REST")))))
      (let (#+nil
            (optional->supplied
             (loop for optional in (create-optional-params &optional)
                collect
                  (cons optional
                        (gensym (symbol-name optional)))))
            #+nil
            (key->supplied
             (loop for key in (create-keyword-params &key)
                collect
                  (cons
                   #-ccl key
                   #+ccl ;; CCL returns keyword symbol and no other info
                   (intern (symbol-name key) wrapping-package)
                   (gensym (symbol-name (if (atom key)
                                            key
                                            (second key))))))))
        `(defun ,(or alt-name
                     (intern (princ-to-string function) wrapping-package))
             (,@(mapcar #'required-param-name required)
              ,@(when &optional 
                      `(&optional
                        ,@(mapcar #'optional-param-whole &optional)))
              ,@(when &rest `(&rest ,@&rest))
              ,@(when &key `(&key ,@(loop for key in key->supplied
                                       collect
                                         (if &rest
                                             (let ((thing (car key)))
                                               (if (atom thing)
                                                   thing
                                                   (list thing)))
                                             `(,(car key) nil ,(cdr key))))))
              ,@(when (member '&allow-other-keys arglist)
                      (if (null &key)
                          `(&key &allow-other-keys)
                          `(&allow-other-keys))))
           ,@(when &rest
                   (list
                    `(declare (ignore ,@(loop for key in key->supplied
                                             collect
                                             (let ((thing (car key)))
                                                     (if (atom thing)
                                                         thing
                                                         (second thing))))))))
           ,(funcall
             body-maker
             function required optional->supplied &rest key->supplied))))))

(defun create-transparent-defun (function wrapper wrapping-package
                                 &key force-rest alt-name)
  (create-transparent-defun%
   function wrapper wrapping-package
   :force-rest force-rest :alt-name alt-name
   :body-maker (lambda (function required optional rest key)
     (funcall wrapper (create-body function required optional rest key)))))

(defmacro transparent-defun (function wrapper wrapping-package
                             &key force-rest alt-name)
  (create-transparent-defun%
   function wrapper wrapping-package
   :force-rest force-rest :alt-name alt-name
   :body-maker (lambda (function required optional rest key)
     `(,wrapper ,(create-body function required optional rest key)))))



;; Macro-wrapping functional arguments should look like this:
;; (lambda (wrapped-macro-form) ``(wrap-something-around ,,wrapped-macro-form))

(defun create-opaque-defmacro (macro wrapper wrapping-package)
  `(defmacro ,(intern (princ-to-string macro) wrapping-package)
       (&rest args)
     ,(funcall wrapper ``(,',macro ,@args))))

;; But macro-wrapping macros should look like normal macros:
;; (defmacro wrapper (wrapped-macro-form)
;;   `(wrap-something-around ,wrapped-macro-form))

(defmacro opaque-defmacro (macro wrapper wrapping-package)
  `(defmacro ,(intern (princ-to-string macro) wrapping-package)
       (&rest args)
     (,wrapper `(,',macro ,@args))))

(defun create-transparent-defmacro (macro wrapper wrapping-package)
  (let* ((arglist (trivial-arguments:arglist (macro-function macro)))
         (whole-p (member '&whole arglist))
         (whole (if whole-p
                    (second whole-p)
                    (gensym "WHOLE"))))
    `(defmacro ,(intern (princ-to-string macro) wrapping-package)
         ,(cond (;; The &whole trick isn't strictly correct because you can
                 ;; still give &whole a destructuring pattern instead of a
                 ;; single variable. But why would anybody do such a thing when
                 ;; that's what the regular arguments are for? Regardless,
                 ;; here's a fallthrough case that gives up on transparency.
                 (listp whole)
                 `(&rest args))
                ((and (symbolp whole)
                      (not (symbol-package whole)))
                 (append `(&whole ,whole) arglist))
                (t arglist))
       ;; to-do: parse actual signature well enough to avoid warnings about
       ;; unused arguments
       ,(if (listp whole)
            (funcall wrapper ``(,',macro ,@args))
            (funcall wrapper
                     ``(,',macro ,@(rest ,whole)))))))

(defmacro transparent-defmacro (macro wrapper wrapping-package)
  (let* ((arglist (trivial-arguments:arglist (macro-function macro)))
         (whole-p (member '&whole arglist))
         (whole (if whole-p
                    (second whole-p)
                    (gensym "WHOLE"))))
    `(defmacro ,(intern (princ-to-string macro) wrapping-package)
         ,(cond (;; same as in function version
                 (listp whole)
                 `(&rest args))
                ((and (symbolp whole)
                      (not (symbol-package whole)))
                 (append `(&whole ,whole) arglist))
                (t arglist))
       ,(if (listp whole)
            `(,wrapper `(,',macro ,@args))
            ;; to-do: parse actual signature well enough to avoid warnings about
            ;; unused arguments
            ``(,',wrapper
               (,',macro ,@(rest ,whole)))))))
