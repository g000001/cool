;;;; package.lisp

(cl:in-package :cl-user)


(defpackage :cool
  (:use)
  (:export))


(defpackage :cool.internal
  (:use :cool :cl :fiveam))

