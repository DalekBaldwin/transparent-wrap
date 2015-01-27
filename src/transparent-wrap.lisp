(in-package :transparent-wrap)

(defparameter *allow-init-forms* nil)
(defparameter *force-rest* nil)

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

(defun real-keyword (key-param)
  (with-slots (name keyword-name) key-param
    (if keyword-name
        keyword-name
        (intern (symbol-name name) :keyword))))

(defun create-body (function required optional rest key init-forms-okay-seq)
  (let* ((&required required)
         (&optional optional)
         (&rest rest)
         (&key key)
         (init-formable-keys
          (loop for key in &key
             when (member key init-forms-okay-seq)
             collect key))
         (non-init-formable-keys
          (set-difference &key init-formable-keys))
         (init-formable-optionals
          (loop for optional in &optional
             when (member optional init-forms-okay-seq)
             collect optional))
         (non-init-formable-optionals
          (set-difference &optional init-formable-optionals))
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
                     (let ((actual-keys
                            (list ,@(loop for key in init-formable-keys
                                       collect (real-keyword key)
                                       collect (key-param-name key)))))
                       (loop
                          for supplied in
                            (list ,@(mapcar
                                     #'key-param-supplied-p-parameter
                                     non-init-formable-keys))
                          for arg in
                            (list ,@(mapcar
                                     #'key-param-name
                                     non-init-formable-keys))
                          for key in
                            (list
                             ,@(loop for key in non-init-formable-keys
                                  collect (real-keyword key)))
                          when supplied
                          do
                            (push arg actual-keys)
                            (push key actual-keys))
                       actual-keys)))
            (t `(,function
                 ,@(mapcar #'required-param-name &required)
                 ,@(mapcar #'optional-param-name &optional))))))
    (if (null non-init-formable-optionals)
        non-optional-portion
        `(case (count-until-false
                (list ,@(loop for optional in non-init-formable-optionals
                           collect (optional-param-supplied-p-parameter optional))))
           ,@(loop for optional in non-init-formable-optionals
                counting optional into i
                collect
                  `(,(1- i)
                     (,function
                      ,@(mapcar #'required-param-name required)
                      ,@(mapcar
                         #'optional-param-name
                         init-formable-optionals)
                      ,@(mapcar
                         #'optional-param-name
                         (subseq non-init-formable-optionals 0 (1- i)))))
                into cases
                finally
                  (return
                    (append
                     cases
                     (list
                      `(otherwise
                        ,non-optional-portion)))))))))

(defun create-transparent-defun% (function wrapper wrapping-package
                                  &key alt-name body-maker)
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
           (&allow-other-keys (find '&allow-other-keys parsed))
           ;; don't use &aux -- luckily it doesn't show up in the reflective
           ;; function signature and its init-forms are the last to be
           ;; evaluated, so we don't lose anything by letting the wrapped
           ;; function handle them completely
           ;; ******************************************************************
           ;; init-forms are guaranteed to be evaluated in left-to-right order,
           ;; so we can safely hoist them into the wrapper only up to the first
           ;; parameter that contains a supply check - hoisting that parameter's
           ;; init-form would cause the wrapped function to see its supplied-p
           ;; as t when it really should be nil, so we can't do that, and
           ;; init-forms further down the line may depend on that variable, so
           ;; we can't hoist any of their init-forms either
           (init-forms-still-okay t)
           (init-forms-okay-seq nil))
      (loop for optional in &optional
         do
           (with-slots (name init-form supplied-p-parameter) optional
             (declare (ignorable init-form))
             #+ccl ;; CCL returns optional parameter name and no other info
             (setf supplied-p-parameter
                   (gensym (concatenate 'string (symbol-name name) "-SUPPLIED")))
             #-ccl
             (cond
               (supplied-p-parameter
                (setf init-form nil
                      init-forms-still-okay nil))
               ((or init-forms-still-okay *allow-init-forms*)
                (setf supplied-p-parameter
                      (gensym (concatenate 'string (symbol-name name) "-SUPPLIED")))
                (push optional init-forms-okay-seq))
               (t
                (setf init-form nil)
                (setf
                 supplied-p-parameter
                 (gensym (concatenate 'string (symbol-name name) "-SUPPLIED")))))))
      (loop for key in &key
         do
           (with-slots (name init-form supplied-p-parameter) key
             (declare (ignorable init-form))
             #+ccl ;; CCL returns keyword symbol and no other info
             (setf name
                     (intern (symbol-name name) wrapping-package)
                   supplied-p-parameter
                     (gensym (concatenate 'string (symbol-name name) "-SUPPLIED")))
             #-ccl
             (cond
               (supplied-p-parameter
                (setf init-form nil
                      init-forms-still-okay nil))
               ((or init-forms-still-okay *allow-init-forms*)
                (setf supplied-p-parameter
                      (gensym (concatenate 'string (symbol-name name) "-SUPPLIED")))
                (push key init-forms-okay-seq))
               (t
                (setf init-form nil)
                (setf supplied-p-parameter
                      (gensym (concatenate 'string (symbol-name name) "-SUPPLIED")))))))
      (setf init-forms-okay-seq (nreverse init-forms-okay-seq))
      (when (and (null &rest)
                 (or *force-rest*
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
                             (with-slots (name init-form supplied-p-parameter)
                                 optional
                               `(,name
                                 ,init-form
                                 ,supplied-p-parameter)))))
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
           function &required &optional &rest &key init-forms-okay-seq)))))

(defun create-transparent-defun (function wrapper wrapping-package
                                 &key ((:force-rest *force-rest*) *force-rest*) alt-name)
  (create-transparent-defun%
   function wrapper wrapping-package
   :alt-name alt-name
   :body-maker
   (lambda (function required optional rest key init-forms-okay-seq)
     (funcall
      wrapper
      (create-body function required optional rest key init-forms-okay-seq)))))

(defmacro transparent-defun (function wrapper wrapping-package
                             &key ((:force-rest *force-rest*) *force-rest*) alt-name)
  (create-transparent-defun%
   function wrapper wrapping-package
   :alt-name alt-name
   :body-maker
   (lambda (function required optional rest key init-forms-okay-seq)
     `(,wrapper
       ,(create-body function required optional rest key init-forms-okay-seq)))))



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
