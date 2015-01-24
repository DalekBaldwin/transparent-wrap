(defpackage :transparent-wrap-test.wrapping
  (:use :cl)
  #.`(:export
      ,@(loop for symbol being the external-symbols of :transparent-wrap-test.original
           when (and (fboundp symbol)
                     (eql (symbol-package symbol) (find-package :transparent-wrap-test.original)))
           collect symbol)))

(in-package :transparent-wrap-test.wrapping)

#.`(progn
     ,@(flet ((wrap (form)
                    `(progn
                       :do-nothing
                       ,form)))
             (loop for symbol being the external-symbols of :transparent-wrap-test.original
                when (and
                      (eql (symbol-package symbol)
                           (find-package :transparent-wrap-test.original))
                      (fboundp symbol))
                collect
                  (transparent-wrap:create-transparent-defun
                   symbol #'wrap :transparent-wrap-test.wrapping))))
