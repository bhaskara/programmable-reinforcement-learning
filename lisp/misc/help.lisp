;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: utils; Base: 10 -*-
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Copyright (c) 2000 The Regents of the University of California.
;;; All rights reserved. 
;;; 
;;; Permission is hereby granted, without written agreement and without
;;; license or royalty fees, to use, copy, modify, and distribute this
;;; software and its documentation for any purpose, provided that the
;;; above copyright notice and the following two paragraphs appear in all
;;; copies of this software.
;;; 
;;; IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY
;;; FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES
;;; ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF
;;; THE UNIVERSITY OF CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.
;;;
;;; THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
;;; INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;; MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE SOFTWARE
;;; PROVIDED HEREUNDER IS ON AN "AS IS" BASIS, AND THE UNIVERSITY OF
;;; CALIFORNIA HAS NO OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES,
;;; ENHANCEMENTS, OR MODIFICATIONS.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Originally written by Tunc Simsek, Univ. of California, Berkeley,
;;; 2000, simsek@eecs.berkeley.edu
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; $Id: help.lisp,v 1.5 2005/11/24 00:31:53 bhaskara Exp $
;;;
;;; $Log: help.lisp,v $
;;; Revision 1.5  2005/11/24 00:31:53  bhaskara
;;; Rewrote print-hash as a macro, made flast more general, added round-decimal, avgf, adjustf, allowed option for help to look at internal symbols, minor changes
;;;
;;; Revision 1.4  2005/10/27 00:27:36  bhaskara
;;; minor
;;;
;;; Revision 1.3  2005/09/29 19:23:44  bhaskara
;;; Changed to only list external symbols of a package
;;;
;;; Revision 1.2  2005/06/29 17:47:05  bhaskara
;;; minor fixes
;;;
;;; Revision 1.1  2005/04/11 18:09:55  bhaskara
;;; *** empty log message ***
;;;
;;; Revision 1.1.1.1  2004/10/15 00:18:39  bhaskara
;;; rereimporting matlisp
;;;
;;; Revision 1.1.1.1  2004/05/24 05:17:45  bhaskara
;;; Imported matlisp
;;;
;;; Revision 1.3  2001/02/21 19:38:01  simsek
;;; o Shortened print width and changed message
;;;
;;; Revision 1.2  2000/07/11 18:02:03  simsek
;;; o Added credits
;;;
;;; Revision 1.1  2000/04/14 00:12:48  simsek
;;; Initial revision.
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package utils)

(defmacro help (&optional (item nil item-p) (include-internals nil))
  "
  Syntax
  ======
  (HELP item)

  Purpose
  =======
  Provides help on ITEM by calling MAN.

  Notes
  =====
  To provide a specialized manual for a function, class etc ...
  you may add a method to MAN.  See MAN.
"
  `(if ,item-p
      (let ((item ',item))
	(typecase item
	  (symbol (man item ,include-internals))
	  (list (if (eq (first item) 'quote)
		    (man (second item) ,include-internals)
		  (dolist (i item)
		    (man i ,include-internals))))
	  (t (error "don't know how to help ~a" item)))
	
	(values))
    (progn
       (format t "~&~%Help is available for the packages")
       (format t   "~%==================================")
       (dolist (p (list-all-packages))
	 (block find-any-doc
	   (do-symbols (s p)
	     (if (equal (symbol-package s) p)
		 (let ((function-doc (documentation s 'function))
		       (variable-doc (documentation s 'variable))
		       (structure-doc (documentation s 'structure))
		       (type-doc (documentation s 'type))
		       (setf-doc (documentation s 'setf)))
		   (if (or (and function-doc
				(not (string= function-doc "")))
			 (and variable-doc
			      (not (string= variable-doc "")))
			 (and structure-doc 
			      (not (string= structure-doc "")))
			 (and type-doc
			      (not (string= type-doc "")))
			 (and setf-doc
			      (not (string= setf-doc ""))))
		       (return-from find-any-doc 
			 (format t "~%~a" (package-name p)))))))))

       (format t "~%~%For help on a particular package try (HELP <package-name>),
For help on a particular symbol try (HELP <symbol-name>). 
For example, (HELP matlisp) (HELP mapcar)")
       (values))))

(defmacro help-int (item)
  "help-int ITEM.  Get help on the internal symbol ITEM."
  `(help ,item t))

(defgeneric man (item &optional include-internals stream)
  (:documentation 
   "
  Syntax
  ======
  (MAN item [include-internals] [stream])

  Purpose
  =======
  Provides the MANual for ITEM, where ITEM is
  any lisp object.

  To provide a specialized manual for a particular
  lisp object you may add a method to MAN.
"))

(defmethod man ((item t) &optional (include-internals nil) (stream t))
  (declare (ignore include-internals))
  (format stream "~&~%No help available for ~a" item))

(defvar *man-term-width* 80
  "Defines the width of the terminal (default value 80)")

(defmethod man ((item package) &optional (include-internals nil) (stream t))
  (declare (ignore include-internals))
  (let ((package-doc (documentation item t)))
    (if package-doc (format stream "~a" package-doc)
      (let ((names nil)
	    (max-length 0)
	    (spacing 2))


	
	(do-external-symbols (s item)
	  (if (equal (symbol-package s) item)
	      (let ((function-doc (documentation s 'function))
		    (variable-doc (documentation s 'variable))
		    (structure-doc (documentation s 'structure))
		    (type-doc (documentation s 'type))
		    (setf-doc (documentation s 'setf)))
		(if (or 
		     (and function-doc
			  (not (string= function-doc "")))
		     (and variable-doc
			  (not (string= variable-doc "")))
		     (and structure-doc 
			  (not (string= structure-doc "")))
		     (and type-doc
			  (not (string= type-doc "")))
		     (and setf-doc
			  (not (string= setf-doc ""))))
		    (push (symbol-name s) names)))))

	(setq names (sort names #'string-lessp))

	(dolist (name names)
	  (if (> (length name) max-length)
	      (setq max-length (length name))))

	(if names
	    (let ((length (length (package-name item))))
	      (format stream "~&~%")
	      (force-output)
	      (format stream 
		      "Package ~a has no documentation of its own.  However, help is available for the following symbols from it"
		      (package-name item))
	      (format stream   
		      "~%================================================")
	      (dotimes (i length)
		(format stream "="))
	      (format stream "~%")))
	(let* ((max (+ spacing max-length))
	       (n (floor (/ *man-term-width* max)))
	       (i 0))
	  (dolist (name names)
	    (let ((length (length name)))
	      (format stream "~a" name)
	      (dotimes (i (- max length))
		(format stream " "))
	      (incf i)
	      (if (> i n)
		  (progn
		    (format stream "~%")
		    (setq i 0))))))))))

(defmethod man ((item symbol) &optional (include-internals nil) (stream t))
  (let ((item-name (symbol-name item))
	(doc-exists-p nil)
	(package (find-package (symbol-name item))))
    
    (flet ((symbol-doc (s p)
	     (if (eq (symbol-package s) p)
		 (if (string= (symbol-name s) item-name)
		     (let ((package (symbol-package s))
			   (function-doc (documentation s 'function))
			   (variable-doc (documentation s 'variable))
			   (structure-doc (documentation s 'structure))
			   (type-doc  (documentation s 'type))
			   (setf-doc (documentation s 'setf)))
		    
		       (dolist (doc (list `(,function-doc "function")
					  `(,variable-doc "variable")
					  `(,structure-doc "structure")
					  `(,type-doc "type")
					  `(,setf-doc "setf")))
			    
			 (if (and (first doc)
				  (not (string= (first doc) ""))
				  (setq doc-exists-p t))
			     (let ((header 
				    (format nil "In package ~a, the ~a ~a"
					    (package-name package)
					    (second doc)
					    (symbol-name s))))
			       (format stream "~&~%~a~%" header)
			       (dotimes (i (length header))
				 (format stream "="))
			       (format stream "~%")
			       (format stream "~a" (first doc)))))
		    
		       )))))
    
      (when package 
	(man package stream))
    
      (if include-internals
	  (dolist (p (list-all-packages))
	    (do-symbols (s p)
	      (symbol-doc s p)))
	(dolist (p (list-all-packages))
	  (do-external-symbols (s p)
	    (symbol-doc s p))))


      (if (or doc-exists-p
	      package)
	  (values)
	(progn
	  (format stream "~&~%No documentation available for symbol ~a"
		  item)
	  (values))))))



