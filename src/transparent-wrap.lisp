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

;; Funcion wrappers should look like:
;; (lambda (form) `(wrap-something-around ,form))

(defun create-basic-defun (function wrapper wrapping-package)
  `(defun ,(intern (princ-to-string function) wrapping-package)
       (&rest args)
     ,(funcall wrapper
               `(apply (symbol-function ',function) args))))

(defun create-transparent-defun (function wrapper wrapping-package)
  (let ((arglist (trivial-arguments:arglist (symbol-function function))))
    
    ;; fall-through: give up on transparency
    (when (eql arglist :unknown)
      (return-from create-transparent-defun (create-basic-defun function wrapper wrapping-package)))
    
    (multiple-value-bind
          (required optional rest key aux)
        (organize-arguments arglist)
      (declare (ignore aux))
      (let ((optional->supplied
             (loop for o in (create-optional-params optional)
                collect
                  (cons o (gensym (symbol-name o)))))
            (key->supplied
             (loop for k in (create-keyword-params key)
                collect
                  (cons k (gensym (symbol-name (if (atom k) k (second k))))))))
        `(defun ,(intern (princ-to-string function) wrapping-package)
             (,@required
              ,@(when optional `(&optional
                                 ,@(loop for o in optional->supplied
                                      collect `(,(car o) nil ,(cdr o)))))
              ,@(when rest `(&rest ,@rest))
              ,@(when key `(&key ,@(loop for k in key->supplied
                                      collect `(,(car k) nil ,(cdr k)))))
              ,@(when (member '&allow-other-keys arglist) `(&allow-other-keys)))
           (case (count-until-false
                  (list ,@(loop for o in optional->supplied
                             collect (cdr o))))
             ,@(loop for o in optional->supplied
                  counting o into i
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
                          ,(cond
                            (rest
                             (funcall
                              wrapper
                              `(apply (symbol-function ',function)
                                      ,@required
                                      ,@(mapcar #'car optional->supplied)
                                      ,@rest)))
                            (key
                             (funcall
                              wrapper
                              `(apply (symbol-function ',function)
                                      ,@required
                                      ,@(mapcar #'car optional->supplied)
                                      (let ((actual-keys))
                                        (loop
                                           for supplied in
                                             (list ,@(loop for k in key->supplied
                                                        collect (cdr k)))
                                           for arg in
                                             (list ,@(loop for k in key->supplied
                                                        collect
                                                          (let ((thing (car k)))
                                                            (if (atom thing)
                                                                thing
                                                                (second thing)))))
                                           for k in
                                             (list ,@(loop for k in key->supplied
                                                        collect
                                                          (let ((thing (car k)))
                                                            (if (atom thing)
                                                                (intern
                                                                 (symbol-name thing)
                                                                 :keyword)
                                                                (first thing)))))
                                           when supplied
                                           do (push k actual-keys) (push arg actual-keys))
                                        (nreverse actual-keys)))))
                            (t (funcall wrapper
                                        `(,function
                                          ,@required
                                          ,@(mapcar #'car optional->supplied))))))))))))))))

;; Macro wrappers should look like:
;; (lambda (form) ``(wrap-something-around ,,form))

(defun create-basic-defmacro (macro wrapper wrapping-package)
  `(defmacro ,(intern (princ-to-string macro) wrapping-package)
       (&rest args)
     ,(funcall wrapper ``(,',macro ,@args))))
