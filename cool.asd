;;;; cool.asd -*- Mode: Lisp;-*- 

(cl:in-package :asdf)

#+:sbcl (require :sb-cltl2)
#+:sbcl (require :sb-introspect)


(defsystem :cool
  :serial t
  :depends-on (:fiveam
               :cool.pcl
               ;#+:sbcl :sb-cltl2
               ;#+:sbcl :sb-introspect
               )
  :components ((:file "package")
               (:file "pcl-patches")
               (:file "co-parse")
               (:file "co-macros")
               (:file "co-dmeth")
               (:file "co-meta")
               (:file "co-dtype")
               (:file "co-sfun")
               #|(:file "co-test")|#
               (:file "co-regress")))


(defmethod perform ((o test-op) (c (eql (find-system :cool))))
  (load-system :cool)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :co-test :co-test))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))

