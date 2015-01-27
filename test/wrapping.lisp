(defpackage :transparent-wrap-test.stateful-wrap
  (:use :cl)
  #.`(:export
      ,@(loop for symbol being the symbols of :transparent-wrap-test.stateful
           when (and (fboundp symbol)
                     (eql (symbol-package symbol) (find-package :transparent-wrap-test.stateful)))
           collect symbol)))

(in-package :transparent-wrap-test.stateful-wrap)

#.`(progn
     ,@(flet ((wrap (form)
                    `(progn
                       :do-nothing
                       ,form)))
             (loop for symbol being the symbols of :transparent-wrap-test.stateful
                when (and
                      (eql (symbol-package symbol)
                           (find-package :transparent-wrap-test.stateful))
                      (fboundp symbol))
                collect
                  (transparent-wrap:create-transparent-defun
                   symbol #'wrap :transparent-wrap-test.stateful-wrap))))

#+nil
(transparent-wrap-test.functional:required-optional-rest-key   a b  )
#+nil
(transparent-wrap-test.functional:required-optional-rest-key   c  )
#+nil
(format t "~&~S~%"
        (transparent-wrap:create-transparent-defun
         'transparent-wrap-test.functional:required-optional-rest-key
         (lambda (form) `(progn :do-nothing ,form))
         :transparent-wrap-test.functional-wrap))


(defpackage :transparent-wrap-test.functional-wrap
  (:use :cl)
  #.`(:export
      ,@(loop for symbol being the symbols of :transparent-wrap-test.functional
           when (and (fboundp symbol)
                     (eql (symbol-package symbol) (find-package :transparent-wrap-test.functional)))
           collect symbol)))

(in-package :transparent-wrap-test.functional-wrap)

#.`(progn
     ,@(flet ((wrap (form)
                    `(progn
                       :do-nothing
                       ,form)))
             (loop for symbol being the symbols of :transparent-wrap-test.functional
                when (and
                      (eql (symbol-package symbol)
                           (find-package :transparent-wrap-test.functional))
                      (fboundp symbol))
                collect
                  (let ((transparent-wrap::*allow-init-forms* t))
                    (transparent-wrap:create-transparent-defun
                     symbol #'wrap :transparent-wrap-test.functional-wrap)))))
