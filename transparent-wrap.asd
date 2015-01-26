;;;; transparent-wrap.asd

(in-package :cl-user)

(defpackage :transparent-wrap-system
  (:use :cl :asdf))
(in-package :transparent-wrap-system)

(defsystem :transparent-wrap
  :name "transparent-wrap"
  :serial t
  :author "Kyle Littler"
  :license "LLGPL"
  :description "A signature-preserving wrapper generator for functions and macros."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :components
  ((:static-file "transparent-wrap.asd")
   (:module :src
            :components ((:file "package")
                         (:file "match")
                         (:file "transparent-wrap"))
            :serial t))
  :depends-on (:trivial-arguments
               :named-readtables
               :optima
               :fare-quasiquote-extras)
  :in-order-to ((test-op (load-op :transparent-wrap-test)))
  :perform (test-op :after (op c)
                    (funcall (intern "TEST-ALL" :transparent-wrap-test))))

(defsystem :transparent-wrap-test
  :name "transparent-wrap-test"
  :serial t
  :components
  ((:module :test
            :components ((:file "package")
                         (:file "original")
                         (:file "wrapping")
                         (:file "transparent-wrap-test"))
            :serial t))
  :depends-on (:transparent-wrap :stefil :alexandria))
