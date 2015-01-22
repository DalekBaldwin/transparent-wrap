(in-package :cl-user)

(defpackage :transparent-wrap
  (:use :cl)
  (:export
   :create-basic-defun
   :create-transparent-defun
   :create-basic-defmacro))

(in-package :transparent-wrap)

(defparameter *system-directory*
  (make-pathname
   :directory
   (pathname-directory
    (asdf:system-definition-pathname :transparent-wrap))))
