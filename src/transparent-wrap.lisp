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
;; (lambda (function-body) `(wrap-something-around ,function-body))

(defun create-basic-defun (function wrapper wrapping-package)
  `(defun ,(intern (princ-to-string function) wrapping-package)
       (&rest args)
     ,(funcall wrapper
               `(apply (symbol-function ',function) args))))

(defun create-transparent-defun (function wrapper wrapping-package)
  (let ((arglist (trivial-arguments:arglist (symbol-function function))))
    
    ;; fall-through: give up on transparency
    (when (eql arglist :unknown)
      (return-from create-transparent-defun
        (create-basic-defun function wrapper wrapping-package)))
    
    (multiple-value-bind
          (required &optional &rest &key &aux)
        (organize-arguments arglist)
      (declare (ignore &aux))
      (let ((optional->supplied
             (loop for optional in (create-optional-params &optional)
                collect
                  (cons optional
                        (gensym (symbol-name optional)))))
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
        `(defun ,(intern (princ-to-string function) wrapping-package)
             (,@required
              ,@(when &optional `(&optional
                                  ,@(loop for optional in optional->supplied
                                       collect `(,(car optional) nil ,(cdr optional)))))
              ,@(when &rest `(&rest ,@&rest))
              ,@(when &key `(&key ,@(loop for key in key->supplied
                                       collect `(,(car key) nil ,(cdr key)))))
              ,@(when (member '&allow-other-keys arglist) `(&allow-other-keys)))
           ,(let ((non-optional-portion
                   (cond
                     (&rest
                      (funcall
                       wrapper
                       `(apply (symbol-function ',function)
                               ,@required
                               ,@(mapcar #'car optional->supplied)
                               ,@&rest)))
                     (&key
                      (funcall
                       wrapper
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
                                    do (progn
                                         (push key actual-keys)
                                         (push arg actual-keys)))
                                 (nreverse actual-keys)))))
                     (t (funcall wrapper
                                 `(,function
                                   ,@required
                                   ,@(mapcar #'car optional->supplied)))))))
                 (if (null &optional)
                     non-optional-portion
                     `(case (count-until-false
                             (list ,@(loop for optional in optional->supplied
                                        collect (cdr optional))))
                        ,@(loop for optional in optional->supplied
                             counting optional into i
                             collect
                               `(,(1- i)
                                  ,(funcall wrapper
                                            `(,function
                                              ,@required
                                              ,@(mapcar
                                                 #'car
                                                 (subseq optional->supplied 0 (1- i))))))
                             into cases
                             finally
                               (return
                                 (append
                                  cases
                                  (list
                                   `(otherwise
                                     ,non-optional-portion)))))))))))))

;; Macro wrappers should look like this:
;; (lambda (macro-body) ``(wrap-something-around ,,macro-body))

(defun create-basic-defmacro (macro wrapper wrapping-package)
  `(defmacro ,(intern (princ-to-string macro) wrapping-package)
       (&rest args)
     ,(funcall wrapper ``(,',macro ,@args))))
