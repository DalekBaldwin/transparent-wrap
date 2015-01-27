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

(defun create-optional-params (optional)
  (remove-duplicates
   (loop for o in optional
      collect
        (if (atom o) o (first o)))))

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

(defun create-body (function required optional rest key)
  (let* ((&required required)
         (&optional optional)
         (&rest rest)
         (&key key)
         (non-optional-portion
          (cond
            (&rest
             `(apply (symbol-function ',function)
                     ,@(mapcar #'required-param-name &required)
                     ,@(mapcar #'optional-param-name &optional)
                     ,@(mapcar #'rest-param-name &rest)))
            (&key
             `(apply (symbol-function ',function)
                     ,@(mapcar #'required-param-name &required)
                     ,@(mapcar #'optional-param-name &optional)
                     (let ((actual-keys))
                       (loop
                          for supplied in
                            (list ,@(mapcar #'key-param-supplied-p-parameter &key))
                          for arg in
                            (list ,@(mapcar #'key-param-name &key))
                          for key in
                            (list ,@(loop for key in &key
                                       collect
                                         (with-slots (name keyword-name) key
                                           (if keyword-name
                                               keyword-name
                                               (intern (symbol-name name) :keyword)))))
                          when supplied
                          do
                            (push arg actual-keys)
                            (push key actual-keys))
                       actual-keys)))
            (t `(,function
                 ,@(mapcar #'required-param-name &required)
                 ,@(mapcar #'optional-param-name &optional))))))
    (if (null &optional)
        non-optional-portion
        `(case (count-until-false
                (list ,@(loop for optional in &optional
                           collect (optional-param-supplied-p-parameter optional))))
           ,@(loop for optional in &optional
                counting optional into i
                collect
                  `(,(1- i)
                     (,function
                      ,@(mapcar #'required-param-name required)
                      ,@(mapcar
                         #'optional-param-name
                         (subseq &optional 0 (1- i)))))
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
           (&required (remove-if-not #'required-param-p parsed))
           (&optional (remove-if-not #'optional-param-p parsed))
           (&rest (remove-if-not #'rest-param-p parsed))
           (&key (remove-if-not #'key-param-p parsed))
           (&allow-other-keys (find '&allow-other-keys parsed)))
      (loop for key in &key
         do
           (with-slots (name init-form supplied-p-parameter)
               key
             ;; CCL returns keyword symbol and no other info
             #+ccl (setf name
                         (intern (symbol-name name) wrapping-package))
             (setf init-form nil)
             (unless supplied-p-parameter
               (setf supplied-p-parameter
                     (gensym (concatenate 'string (symbol-name name) "-SUPPLIED"))))))
      (loop for optional in &optional
         do
           (with-slots (name init-form supplied-p-parameter) optional
             (setf init-form nil)
             (unless supplied-p-parameter
               (setf supplied-p-parameter
                     (gensym (concatenate 'string (symbol-name name) "-SUPPLIED"))))))
      (when (and (null &rest)
                 (or force-rest
                     ;; MUST pass possibly unknown args through
                     &allow-other-keys))
        (setf &rest (list (make-rest-param :name (gensym "REST")))))
      `(defun ,(or alt-name
                   (intern (princ-to-string function) wrapping-package))
           (,@(mapcar #'required-param-name &required)
            ,@(when &optional 
                    `(&optional
                      ,@(loop for optional in &optional
                           collect
                             `(,(optional-param-name optional)
                                ,(optional-param-init-form optional)
                                ,(optional-param-supplied-p-parameter optional)))))
            ,@(when &rest `(&rest ,@(mapcar #'rest-param-name &rest)))
            ,@(when &key
                    `(&key
                      ,@(loop for key in &key
                           collect
                             (with-slots
                                   (name keyword-name init-form supplied-p-parameter)
                                 key
                               `(,(if keyword-name
                                      (list keyword-name name)
                                      name)
                                  ,init-form
                                  ,supplied-p-parameter)))))
            ,@(when &allow-other-keys
                    `(&allow-other-keys)))
         ,@(when &rest
                 (list
                  `(declare (ignore ,@(mapcar #'key-param-name &key)))))
         ,(funcall
           body-maker
           function &required &optional &rest &key)))))

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
