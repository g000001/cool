;;;; cool.asd -*- Mode: Lisp;-*- 

(cl:in-package :asdf)


(defsystem :cool.pcl
  :serial t
  :depends-on (:fiveam)
  :components ((:file "pcl-package")
               (:file "cltl1-compat")
               #-sbcl (:file "pcl-walk")
               #-sbcl (:file "pcl-walk-test")
               (:file "pcl-macros")
               (:file "pcl-low")
               #+sbcl (:file "pcl-sbcl-low")
               (:file "pcl-braid")
               (:file "pcl-class-slots")
               (:file "pcl-defclass")
               (:file "pcl-class-prot")
               (:file "pcl-methods")
               (:file "pcl-dfun-templ")
               (:file "pcl-fixup")
               (:file "pcl-high")
               (:file "pcl-compat")))


(defsystem :cool
  :serial t
  :depends-on (:fiveam :cool.pcl)
  :components ((:file "package")
               (:file "pcl-patches")
               (:file "co-macros")
               (:file "co-dmeth")
               (:file "co-meta")
               (:file "co-dtype")
               (:file "co-sfun")
               (:file "co-test")
               (:file "co-regress")))


(defmethod perform ((o test-op) (c (eql (find-system :cool))))
  (load-system :cool)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :cool.internal :cool))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))

