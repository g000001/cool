;;;-*-Mode:LISP; Package:(PCL LISP 1000); Base:10; Syntax:Common-lisp -*-
;;;
;;; *************************************************************************
;;; Copyright (c) 1985, 1986, 1987 Xerox Corporation.  All rights reserved.
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
;;; Some support stuff for compiling and loading PCL.  It would be nice if
;;; there was some portable make-system we could all agree to share for a
;;; while.  At least until people really get databases and stuff.
;;;
;;; *** To install PCL at a new site, read the directions above the    ***
;;; *** second and third defvars in this file (down about 10 lines).  ***
;;;

(in-package 'pcl :use (list (or (find-package 'walker)
				(make-package 'walker :use '(lisp)))
			    'lisp))

(defvar *pcl-system-date* "2/24/87")

;;;
;;; Some CommonLisps have more symbols in the Lisp package than the ones that
;;; are explicitly specified in CLtL.  This causes trouble. Any Lisp that has
;;; extra symbols in the Lisp package should shadow those symbols in the PCL
;;; package.
;;;
#+TI
(shadow '(string-append once-only destructuring-bind
	  memq assq delq neq ignore true false
	  without-interrupts
	  defmethod)
	'pcl)
#+Spice
(shadow '(memq assq delq) (find-package 'pcl))
#+Symbolics
(shadow '(ignore) (find-package 'pcl))

;;;
;;; When installing PCL at your site, edit this defvar to give the directory
;;; in which the PCL files are stored.  The values given below are EXAMPLES
;;; of correct values for *pcl-pathname-defaults*.
;;; 
(defvar *pcl-pathname-defaults*
	#+Symbolics                (pathname "avalon:>Gregor>pcl>")
	#+SUN                      (pathname "/usr/yak/gregor/pcl/")
	#+ExCL                     (pathname "/usr/yak/gregor/pcl/")
	#+KCL                      (pathname "/user/isl/gregor/pcl/")
	#+(and DEC common vax VMS) (pathname "[gregor]")
	#+Spice		           (pathname "pcl:")
	#+HP                  (pathname "/net/hplfs2/users/kempf/public/pcl/")
	#+Xerox                    (pathname "{phylum}<pcl>")
	)

;;;
;;; When you get a copy of PCL (by tape or by FTP), the sources files will
;;; have extensions of ".l" specifically, this file will be named defsys.l.
;;; The preferred way to install pcl is to rename these files to have the
;;; extension which your lisp likes to use for its files.  Alternately, it
;;; is possible not to rename the files.  If the files are not renamed to
;;; the proper convention, the second line of the following defvar should
;;; be changed to:
;;; 	(let ((files-renamed-p nil)
;;;
;;; Note: Something people installing PCL on a machine running Unix
;;;       might find useful.  If you want to change the extensions
;;;       of the source files from ".l" to ".lsp", *all* you have to
;;;       do is the following:
;;;
;;;       % foreach i (*.l)
;;;       ? mv $i $i:r.lsp
;;;       ? end
;;;       %
;;;
;;;       I am sure that a lot of people already know that, and some
;;;       Unix hackers may say, "jeez who doesn't know that".  Those
;;;       same Unix hackers are invited to fix mv so that I can type
;;;       "mv *.l *.lsp".
;;;
(defvar *pathname-extensions*
	(let ((files-renamed-p t)
	      (proper-extensions
		(car '(#+Symbolics           ("lisp"  . "bin")
		       #+(and dec common)    ("LSP"   . "FAS")
		       #+KCL                 ("lsp"   . "o")
		       #+Xerox               ("lisp"  . "dfasl")
		       #+(and Lucid MC68000) ("lisp"  . "lbin")
		       #+(and Lucid VAX VMS) ("lisp"  . "vbin")
		       #+excl                ("cl"    . "fasl")
		       #+Spice               ("slisp" . "sfasl")
		       #+HP                  ("l"     . "b")
		       #+TI                  ("lisp"  . "xfasl")
		       ))))
	  (cond ((null proper-extensions) '("l" . "lbin"))
		((null files-renamed-p) (cons "l" (cdr proper-extensions)))
		(t proper-extensions))))



;;;
;;; *PCL-FILES* is a kind of "defsystem" for pcl.  A new port of pcl should
;;; add an entry for that port's xxx-low file.
;;; 
(defvar *pcl-files*
  (let ((xxx-low (or #+Symbolics '3600-low
		     #+Lucid     'lucid-low
		     #+Xerox     'Xerox-low
		     #+TI        'ti-low
		     #+(and dec common) 'vaxl-low
		     #+KCL       'kcl-low
		     #+excl      'excl-low
		     #+Spice     'spice-low
		     #+HP        'hp-low
		     nil)))
    ;; file         load           compile         files which force
    ;;              environment    environment     recompilations of
    ;;                                             this file
    `(
      #+Symbolics
      (rel-7-patches nil            nil                    nil)
      #+Symbolics
      (walk         (rel-7-patches) (rel-7-patches)        nil)
      #-Symbolics
      (walk         nil             nil                    ())
      (macros       (walk)          (walk macros)          ())
      (low          (walk)          (macros)               (macros))
      (,xxx-low     (low)           (macros low)           ())
      (braid        t               ((braid :source))      (low ,xxx-low))
      (class-slots  t               (braid)                (low ,xxx-low))
      (defclass     t               (braid defclass)       (low ,xxx-low))
      (class-prot   t               (braid
				     defclass)             (low ,xxx-low))
      (methods      t               (braid
				     class-prot
				     (methods :source)	;Because Common Lisp
						        ;makes it unlikely
						        ;that any particular
						        ;CommonLisp will do
						        ;the right thing with
						        ;a defsetf during
						        ;a compile-file.
				     )                  (low ,xxx-low))
      (dfun-templ   t               (methods 
				      (dfun-templ :source)) (low ,xxx-low))
      (fixup        t               (braid
				     methods
				     (fixup :source))   (low
							 ,xxx-low
							 braid
							 class-slots
							 defclass
							 class-prot
							 methods
							 dfun-templ))
      (high         (fixup)         ((high :source))    (low ,xxx-low walk))
      (compat       (high)          (high))
;     (meth-combi   (high)          (high)              )
;     (meth-combs   (meth-combi)    (meth-combi)        (meth-combi))
;     (trapd        (meth-combs)    (high)              )
      )))

(defun load-pcl (&optional (sources-p nil))
  (load-system
    (if sources-p :sources :load) *pcl-files* *pcl-pathname-defaults*)
  (provide "pcl"))

(defun compile-pcl (&optional (force-p nil))
  (load-system (if force-p ':force ':compile) *pcl-files* *pcl-pathname-defaults*))

  ;;   
;;;;;; load-system
  ;;
;;; Yet Another Sort Of General System Facility and friends.
;;; 

(defstruct (module (:constructor make-module
				 (name load-env comp-env recomp-reasons))
		   (:print-function
		     (lambda (m s d)
		       (declare (ignore d))
		       (format s
			       "#<Module ~A L:~@A  C:~@A  R:~@A>"
			       (module-name m)
			       (module-load-env m)
			       (module-comp-env m)
			       (module-recomp-reasons m)))))
  name
  load-env
  comp-env
  recomp-reasons)

(defun load-system (mode system *default-pathname-defaults*)
  (#+Symbolics compiler:compiler-warnings-context-bind
   #-Symbolics progn
   (let ((loaded ())    ;A list of the modules loaded so far.
	 (compiled ())  ;A list of the modules we have compiled.
	 (modules ())   ;All the modules in the system.
	 (module-names ())
	 (*modules-to-source-load* ()))
     (declare (special *modules-to-source-load*))
     (labels
       (
       ;(load (x) x)
       ;(compile-file (x) x)
	(find-module (name)
	  (or (car (member name modules :key #'module-name))
	      (error "Can't find module of name ~S???" name)))
	(needs-compiling-p (m)
	  (or (null (probe-file (make-binary-pathname (module-name m))))
	      (eq (module-recomp-reasons m) 't)
	      (dolist (r (module-recomp-reasons m))
		(when (member (find-module r) compiled)
		  (return t)))
	      (> (file-write-date (make-source-pathname (module-name m)))
		 (file-write-date (make-binary-pathname (module-name m))))))
	(compile-module (m)
	  (unless (member m compiled)
	    (assure-compile-time-env m)
	    (format t "~&Compiling ~A..." (module-name m))
	    (compile-file (make-source-pathname (module-name m)))
	    (push m compiled)))
	(load-module (m &optional source-p)
	  (setq source-p (or (if (member m *modules-to-source-load*) t nil)
			     source-p
			     (eq mode :sources)))
	  (unless (dolist (l loaded)
		    (and (eq (car l) m)
			 (eq (cdr l) source-p)
			 (return t)))
	    (assure-load-time-env m)
	    (cond (source-p
		   (format t "~&Loading source of ~A..." (module-name m))
		   (load (make-source-pathname (module-name m))))
		  (t
		   (format t "~&Loading ~A..." (module-name m))
		   (load (make-binary-pathname (module-name m)))))
	    (push (cons m source-p) loaded)))
	(assure-compile-time-env (m)
	  (let ((*modules-to-source-load*
		  (cons m *modules-to-source-load*)))
	    (declare (special *modules-to-source-load*))	;Should not have to
						;but...
	    (dolist (c (module-comp-env m))
	      (when (eq (cadr c) :source)
		(push (find-module (car c)) *modules-to-source-load*)))
	    (dolist (c (module-comp-env m))
	      (load-module (find-module (car c))))))
	(assure-load-time-env (m)
	  (dolist (l (module-load-env m))
	    (load-module (find-module l))))
	)
       
       ;; Start by converting the list representation of we got into
       ;; modules.  At the same time, we convert the abbreviations
       ;; for load-envs and comp envs to the unabbreviated internal
       ;; representation.
       (dolist (file system)
	 (let ((name (car file))
	       (load-env (cadr file))
	       (comp-env (caddr file))
	       (recomp-reasons (cadddr file)))
	   (push (make-module name
			      (if (eq load-env 't)
				  (reverse module-names)
				  load-env)
			      (mapcar #'(lambda (c)
					  (if (listp c)
					      c
					      (list c :binary)))
				      (if (eq comp-env 't)
					  (reverse (cons name module-names))
					  comp-env))
			      recomp-reasons)
		 modules)
	   (push name module-names)))
       (setq modules (nreverse modules))
       (ecase mode
	 (:compile
	   (dolist (module modules)
	     (when (needs-compiling-p module)
	       (compile-module module))))
	 (:force
	   (dolist (module modules)
	     (compile-module module)))
	 (:load
	   (dolist (module modules)
	     (load-module module)))
	 (:sources
	   (dolist (module modules)
	     (load-module module t))))))))

(defun make-source-pathname (name)
  (make-pathname
    :name #-VMS (string-downcase (string name))
          #+VMS (string-downcase (substitute #\_ #\- (string name)))
    :type (car *pathname-extensions*)
    :defaults *default-pathname-defaults*))

(defun make-binary-pathname (name)
  (make-pathname
    :name #-VMS (string-downcase (string name))
          #+VMS (string-downcase (substitute #\_ #\- (string name)))
    :type (cdr *pathname-extensions*)
    :defaults *default-pathname-defaults*))

