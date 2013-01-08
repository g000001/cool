(in-package :cool.pcl.walker)

(defmacro cltl1-eval-when ((&rest args) &body body)
  `(eval-when (,@(mapcar (lambda (k)
                           (case k
                             (compile :compile-toplevel)
                             (load :load-toplevel)
                             (eval :execute)))
                         args))
     ,@body))


;; (defun cltl1-error )

;; (cltl1-error )

(defun evalhook (&rest args)
  (format t "~{A~^ ~}~%" (list args)))


