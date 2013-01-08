;;; -*- Mode:LISP; Package:(PCL Lisp 1000); Base:10.; Syntax:Common-lisp -*-
;;;
;;; *************************************************************************
;;; Copyright (c) 1985 Xerox Corporation.  All rights reserved.
;;;
;;; Use and copying of this software and preparation of derivative works
;;; based upon this software are permitted.  Any distribution of this
;;; software or derivative works must comply with all applicable United
;;; States export control laws.
;;; 
;;; This software is made available AS IS, and Xerox Corporation makes no
;;; warranty about the software, its performance or its conformity to any
;;; specification.
;;; 
;;; Any person obtaining a copy of this software is requested to send their
;;; name and post office or electronic mail address to:
;;;   CommonLoops Coordinator
;;;   Xerox Artifical Intelligence Systems
;;;   2400 Hanover St.
;;;   Palo Alto, CA 94303
;;; (or send Arpanet mail to CommonLoops-Coordinator.pa@Xerox.arpa)
;;;
;;; Suggestions, comments and requests for improvements are also welcome.
;;; *************************************************************************
;;;
;;; This is the 1100 (Xerox version) of the file portable-low.
;;;

(in-package 'pcl)

(defmacro load-time-eval (form)
  `(il:LOADTIMECONSTANT ,form))

  ;;   
;;;;;; Memory block primitives.
  ;;

; what I have done is to replace all calls to il:\\RPLPTR with a call to
; RPLPTR (in the PCL) package.  RPLPTR is a version which does some error
; checking and then calls il:\\RPLPTR.  As follows:

;(defun rplptr (block index value)
;  (if (< index (* (il:\\#blockdatacells block) 2))
;      (il:\\rplptr block index value)
;      (error "bad args to rplptr")))

(defmacro make-memory-block (size &optional area)
  `(il:\\allocblock ,size T))

(defmacro memory-block-ref (block offset)
  `(il:\\GETBASEPTR ,block (* ,offset 2)))

(defsetf memory-block-ref (memory-block offset) (new-value)
  `(il:\\rplptr ,memory-block (* ,offset 2) ,new-value))

(defmacro memory-block-size (block)
  ;; this returns the amount of memory allocated for the block --
  ;; it may be larger than size passed at creation
  `(il:\\#BLOCKDATACELLS ,block))

(defmacro CLEAR-memory-block (block start)
  (once-only (block)
    `(let ((end (* (il:\\#blockdatacells ,block) 2)))
       (do ((index (* ,start 2) (+ index 2)))
	   ((= index end))
	 (il:\\rplptr ,block index nil)))))

  ;;   
;;;;;; Static slot storage primitives
  ;;   

;;;
;;; Once everything sees to work, uncomment this back into play and remove
;;; the * 2 in the other places.
;;; 
;(defmacro %convert-slotd-position-to-slot-index (slotd-position)
; `(* 2 ,slotd-position))

(defmacro %allocate-static-slot-storage--class (no-of-slots)
  `(il:\\ALLOCBLOCK ,no-of-slots t))

(defmacro %static-slot-storage-get-slot--class (static-slot-storage
						slot-index)
  `(il:\\GETBASEPTR ,static-slot-storage (* ,slot-index 2)))

(defsetf %static-slot-storage-get-slot--class (static-slot-storage
					       slot-index)
					      (new-value)
  `(il:\\rplptr ,static-slot-storage (* ,slot-index 2) ,new-value))


  ;;   
;;;;;; Instance With Meta-Class Class (IWMC-CLASS)
  ;;   
;;; In Xerox Lisp, the type of an object is inextricably linked to its size.
;;; This means that we can't build IWMC-CLASS on top of %instance and still
;;; get rid of the indirection to instance-storage the way we would like to.
;;; So, we build iwmc-class on its own base using defstruct.
;;;
;;; NOTE: %instance-meta-class will not return the right value for an
;;;       instance

(eval-when (compile load eval)
  ;; see if we can save our implementation of macros from itself
  (dolist (x '(iwmc-class-class-wrapper
	       iwmc-class-static-slots
	       iwmc-class-dynamic-slots))
    (fmakunbound x)
    (remprop x 'il:macro-fn)))

(defstruct (iwmc-class (:predicate iwmc-class-p)
		       (:conc-name iwmc-class-)
		       (:constructor %%allocate-instance--class ())
		       (:print-function print-instance))
  (class-wrapper nil)
  (static-slots nil)
  (dynamic-slots ()))

(defmacro %allocate-instance--class (no-of-slots &optional class-class)
  `(let ((iwmc-class (%%allocate-instance--class)))
     (%allocate-instance--class-1 ,no-of-slots iwmc-class)
     iwmc-class))


(defmacro %allocate-class-class (no-of-slots)	;This is used to allocate the
  `(let ((i (%%allocate-instance--class)))	;class class.  It bootstraps
    ;(setf (%instance-meta-class i) i)		;the call to class-named in
     (setf (class-named 'class) i)		;%allocate-instance--class.
     (%allocate-instance--class-1 ,no-of-slots i)
     i))

(eval-when (compile load eval)
  (setq *class-of*
	'(lambda (x) 
	   (or (and (iwmc-class-p x)
		    (class-of--class x))
	       (and (%instancep x)
		    (%instance-class-of x))
	      ;(%funcallable-instance-p x)
	       (class-named (type-of x) t)
	       (error "Can't determine class of ~S" x))))

  (setq *meta-classes* (delete (assq 'class *meta-classes*) *meta-classes*)))



  ;;   
;;;;;; FUNCTION-ARGLIST
  ;;

(defun function-arglist (x) (il:arglist x))

  ;;   
;;;;;; Generating CACHE numbers
  ;;

(defmacro symbol-cache-no (symbol mask)
  `(logand (il:llsh (logand #o17777 (il:\\loloc ,symbol)) 2) ,mask))

(defmacro object-cache-no (object mask)
  `(logand (il:\\loloc ,object) ,mask))


  ;;   
;;;;;; printing-random-thing-internal
  ;;

(defun printing-random-thing-internal (thing stream)
  (princ (il:\\hiloc thing) stream)
  (princ "," stream)
  (princ (il:\\loloc thing) stream))

(defun record-definition (name type &optional parent-name parent-type)
  (declare (ignore type parent-name))
  ())

