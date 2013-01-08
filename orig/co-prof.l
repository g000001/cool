
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         co-prof.l
; SCCS:         %A% %G% %U%
; Description:  Profiling For COOL
; Author:       James Kempf, HP/DCC
; Created:      10-Feb-87
; Modified:     25-Feb-87 10:51:31 (James Kempf)
; Language:     Lisp
; Package:      TEST
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package 'test)

(require "co")

(use-package 'co)

(require "co-profmacs")

;;Collection Variable for Test Functions

(defvar *function-symbols* NIL)

;;Default names for output file and output messages.
;;  Can be overridden before this file is loaded.

(defvar *output-file-name* "runprof.out")
(defvar *definition-message* "COOL Definition Results")
(defvar *redefinition-message* "COOL Redefinition Results")

;;Run everything compiled so that best
;;  times are obtained.

;;Measurement of Type Definition

;;Warmup

(do-type-definition NIL 0 0)
(compile (first *function-symbols*))
(funcall (first *function-symbols*))

;;No instance variables and no parents

(do-type-definition T 0 0)
(compile (first *function-symbols*))
(funcall (first *function-symbols*))

;;One instance variable and no parents

(do-type-definition T 1 0)
(compile (first *function-symbols*))
(funcall (first *function-symbols*))

;;Two instance variables and no parents

(do-type-definition T 2 0)
(compile (first *function-symbols*))
(funcall (first *function-symbols*))

;;Three instance variables and no parents

(do-type-definition T 3 0)
(compile (first *function-symbols*))
(funcall (first *function-symbols*))

;;No variables and one parent

(do-type-definition T 0 1)
(compile (first *function-symbols*))
(funcall (first *function-symbols*))

;;No variables and two parents

(do-type-definition T 0 2)
(compile (first *function-symbols*))
(funcall (first *function-symbols*))

;;No variables and three parents

(do-type-definition T 0 3)
(compile (first *function-symbols*))
(funcall (first *function-symbols*))

;;Measure Instance Creation

;;Warmup

(do-instance-creation NIL 0 0)
(compile (first *function-symbols*))
(funcall (first *function-symbols*))

;;No instance variables and no parents

(do-instance-creation T 0 0)
(compile (first *function-symbols*))
(funcall (first *function-symbols*))

;;One instance variable and no parents

(do-instance-creation T 1 0)
(funcall (first *function-symbols*))

;;Two instance variables and no parents

(do-instance-creation T 2 0)
(compile (first *function-symbols*))
(funcall (first *function-symbols*))

;;Three instance variables and no parents

(do-instance-creation T 3 0)
(compile (first *function-symbols*))
(funcall (first *function-symbols*))

;;No variables and one parent

(do-instance-creation T 0 1)
(compile (first *function-symbols*))
(funcall (first *function-symbols*))

;;No variables and two parents

(do-instance-creation T 0 2)
(compile (first *function-symbols*))
(funcall (first *function-symbols*))

;;No variables and three parents

(do-instance-creation T 0 3)
(compile (first *function-symbols*))
(funcall (first *function-symbols*))

;;Measurement of Method Definition

(do-method-definition NIL 0 temp1)
(compile (first *function-symbols*))
(funcall (first *function-symbols*))

;;So that new symbols will be generated

(setf *list-of-method-symbols* NIL)

;;No predefined method

(do-method-definition T 0 temp1)
(compile (first *function-symbols*))
(funcall (first *function-symbols*))

;;Measure method invocation

(do-messaging T 1 temp1)
(compile (first *function-symbols*))
(funcall (first *function-symbols*))

;;One predefined method

(do-method-definition T 1 temp2)
(compile (first *function-symbols*))
(funcall (first *function-symbols*))

;;Measure method invocation

(do-messaging T 2 temp1 temp2)
(compile (first *function-symbols*))
(funcall (first *function-symbols*))

;;Two predefined methods

(do-method-definition T 2 temp3)
(compile (first *function-symbols*))
(funcall (first *function-symbols*))

;;Measure method invocation

(do-messaging T 3 temp1 temp2 temp3)
(compile (first *function-symbols*))
(funcall (first *function-symbols*))

;;Three predefined methods

(do-method-definition T 3 temp4)
(compile (first *function-symbols*))
(funcall (first *function-symbols*))

;;Measure method invocation

(do-messaging T 4 temp1 temp2 temp3 temp4)
(compile (first *function-symbols*))
(funcall (first *function-symbols*))

;;Method Invocation and Inheritence

(do-inherited-messaging NIL 0 g0f)
(compile (first *function-symbols*))
(funcall (first *function-symbols*))

;;No inheritence

(do-inherited-messaging T 0 g0f)
(compile (first *function-symbols*))
(funcall (first *function-symbols*))


;;One level

(do-inherited-messaging T 1 g1f)
(compile (first *function-symbols*))
(funcall (first *function-symbols*))


;;Two levels

(do-inherited-messaging T 2 g2f)
(compile (first *function-symbols*))
(funcall (first *function-symbols*))


;;Three levels

(do-inherited-messaging T 3 g3f)
(compile (first *function-symbols*))
(funcall (first *function-symbols*))


;;Dump out the results

(print-results *output-file-name* *definition-message*)

;;Run Everything Again

(dolist (l (reverse *function-symbols*))
  (funcall l)
)

;;And dump results

(print-results *output-file-name* *redefinition-message*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide "co-prof")

