(in-package :cl-user)

(defpackage :transparent-wrap
  (:use :cl)
  (:export
   :create-opaque-defun
   :create-transparent-defun
   :transparent-defun
   :create-opaque-defmacro))

(in-package :transparent-wrap)

(defparameter *system-directory*
  (make-pathname
   :directory
   (pathname-directory
    (asdf:system-definition-pathname :transparent-wrap))))
