(in-package :cl-user)

(defpackage :transparent-wrap-test
  (:use :cl :transparent-wrap :stefil)
  (:export
   #:test-all))

(in-package :transparent-wrap-test)

(defparameter *system-directory*
  (make-pathname
   :directory 
   (pathname-directory
    (asdf:system-definition-pathname :transparent-wrap))))
