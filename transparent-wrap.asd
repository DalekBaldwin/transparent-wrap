;;;; transparent-wrap.asd

(defpackage :transparent-wrap-system
  (:use :cl :asdf))
(in-package :transparent-wrap-system)

(defsystem :transparent-wrap
  :name "transparent-wrap"
  :serial t
  :components
  ((:static-file "transparent-wrap.asd")
   (:module :src
            :components ((:file "package")
                         (:file "transparent-wrap"))
            :serial t))
  :depends-on (:trivial-arguments))

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
