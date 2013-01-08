;;;; cool.asd -*- Mode: Lisp;-*- 

(cl:in-package :asdf)

(defsystem :cool
  :serial t
  :depends-on (:fiveam)
  :components ((:file "package")
               (:file "cool")))

(defmethod perform ((o test-op) (c (eql (find-system :cool))))
  (load-system :cool)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :cool.internal :cool))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))

