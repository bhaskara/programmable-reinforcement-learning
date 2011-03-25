(in-package utils)


(defmacro with-gensyms (syms &body body)
  "set the value of each symbol in SYMS to a unique gensym"
  `(let ,(mapcar #'(lambda (s)
		     `(,s (gensym)))
	  syms)
     ,@body))


(defmacro eval-now (&body body)
  "macro EVAL-NOW.  Expands to (eval-when (:compile-toplevel :load-toplevel :execute) ,@body)"
  `(eval-when (:compile-toplevel :load-toplevel :execute) ,@body))

(defmacro with-outfile ((f filename) &body body)
  "with-outfile (VAR FILENAME) &body BODY.  Call body with VAR bound to a stream that writes to FILENAME.  A shorthand for with-open-file that does output and overwrites the file if it exists already."
  `(with-open-file (,f ,filename :direction :output :if-exists :supersede) ,@body))

(defun mappend (fn &rest lsts)
  (apply #'append (apply #'mapcar fn lsts)))

(defmacro while (test &rest body)
  "while TEST &rest BODY"
  `(loop
       while ,test
       do ,@ body))

(defmacro until (test &rest body)
  "until TEST &rest BODY"
  `(while (not ,test) ,@body))


(defmacro rand-fn-hist (f n &key (inc (ceiling n 100)) (test #'equalp))
  "rand-fn-hist F N &key INC TEST calls the form F N times, printing a . for progress every INC steps, and displays (and returns) the results as a hash-table using TEST (default #'equalp)"
  `(loop
       with val = nil
       with h = (make-hash-table :test ,test)
		
       for i from 1 to ,n
		  
       if (zerop (mod i ,inc))
       do (format t ".")
		  
       do (setf val ,f)
       do (setf (gethash val h)
	    (1+ (gethash val h 0)))
	 
       finally (return h)))



  


  
  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; abbreviations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro abbrev (x y)
  (let ((args (gensym))
	(doc-string (format nil "~a : Abbreviation macro for ~a" x y)))

    `(defmacro ,x (&rest ,args)
       ,doc-string
       `(,',y ,@,args))))

(abbrev mvbind multiple-value-bind)
(abbrev mvsetq multiple-value-setq)
(abbrev dbind destructuring-bind)
(abbrev unbind-slot slot-makunbound)

       

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; assertions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro verify-type (x typespec msg &rest args)
  "verify-type X TYPESPEC MSG &rest ARGS.  First verify that X satisfies TYPESPEC (not evaluated).  If it does, return it.  If not, assert with MSG and ARGS."
  (let ((y (gensym)))
    `(let ((,y ,x))
       (check-type ,y ,typespec ,msg ,@args)
       ,y)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setf macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro _f (op place &rest args)
  "_f OP PLACE &rest ARGS.  A correct (i.e. multiple-evaluation-avoiding) version of (setf PLACE (apply OP PLACE ARGS))"
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)
    `(let* (,@(mapcar #'list vars forms)
	    (, (car var) (,op ,access ,@args)))
       ,set)))

(defun avg (x y weight)
  "avg X Y WEIGHT.  Assumes WEIGHT is between 0 and 1.  Returns WEIGHT*Y + (1-WEIGHT)*X."
  (+ (* y weight) (- 1 weight) x))

(defmacro avgf (place new-val weight)
  "avgf PLACE NEW-VAL W.  Replace PLACE with W*NEW-VAL + (1-W)*PLACE."
  `(_f avg ,place ,new-val ,weight))

(defmacro adjustf (place &rest args)
  "adjustf PLACE &rest ARGS.  Applies adjust-array to PLACE with ARGS and stores the new array in PLACE."
  `(_f adjust-array ,place ,@args))

(defmacro multf (place &rest args)
  "multf PLACE &rest ARGS.  Multiplies PLACE by the ARGS and stores the result back in place."
  `(_f * ,place ,@args))

(defmacro maxf (place &rest args)
  "maxf PLACE &rest ARGS.  Setf PLACE to max(PLACE, max(args))."
  `(_f max ,place ,@args))

(defmacro orf (place &rest args)
  "orf PLACE &rest ARGS.  Setf PLACE to (or PLACE . ARGS)."
  `(_f or ,place ,@args))

(defun my-delete (place item &rest args)
  (apply #'delete item place args))

(defun my-adjoin (place item &rest args)
  (apply #'adjoin item place args))

(defmacro deletef (place item &rest args)
  "deletef PLACE ITEM &rest ARGS.  Setf place to (delete item place . args)"
  `(_f my-delete ,place ,item ,@args))
  
(defmacro adjoinf (place item &rest args)
  "adjoinf PLACE ITEM &rest ARGS.  Setf place to (adjoin ITEM PLACE . ARGS)"
  `(_f my-adjoin ,place ,item ,@args))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; condlet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro condlet (clauses &body body)
  "condlet CLAUSES &rest BODY.  CLAUSES is a list of which each member is a conditional binding or otherwise clause.  There can be at most one otherwise clause and it must be the last clause.  Each conditional binding is a list where the first element is a test and the remaining elements are the bindings to be made if the test succeeds.   Each clause must bind the same set of variables.  If one of the tests succeeds, the corresponding bindings are made, and the body evaluated.  If none of the tests suceeds, the otherwise clause, if any, is evaluated instead of the body."
  (let* ((var-names (mapcar #'car (cdr (first clauses))))
	 (otherwise-clause? (eql (caar (last clauses)) 'otherwise))
	 (actual-clauses (if otherwise-clause? (butlast clauses) clauses)))
    (assert (every (lambda (cl) (equal var-names (mapcar #'car (cdr cl))))
		   actual-clauses)
	nil "All non-otherwise-clauses in condlet must have same variables.")
    (let ((bodfn (gensym))
	  (vars (mapcar (lambda (v) (cons v (gensym)))
			var-names)))
      `(labels ((,bodfn ,(mapcar #'car vars)
		  ,@body))
	 (cond 
	  ,@(mapcar (lambda (cl) (condlet-clause vars cl bodfn))
		    actual-clauses)
	  ,@(when otherwise-clause? `((t (progn ,@(cdar (last clauses)))))))))))

(defun condlet-clause (vars cl bodfn)
  `(,(car cl) (let ,(condlet-binds vars cl)
		(,bodfn ,@(mapcar #'cdr vars)))))

(defun condlet-binds (vars cl)
  (mapcar (lambda (bindform)
	    (if (consp bindform)
		(cons (cdr (assoc (car bindform) vars))
		      (cdr bindform))))
	  (cdr cl)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; anaphoric macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro awhen (test-form &body body)
  `(aif ,test-form
	(progn ,@body)))

(defmacro awhile (expr &body body)
  `(do ((it ,expr ,expr))
       ((not it))
     ,@body))

(defmacro aand (&rest args)
  (cond ((null args) t)
	((null (cdr args)) (car args))
	(t `(aif ,(car args) (aand ,@(cdr args))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; testing macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *lhs*)
(defvar *rhs*)

(defmacro do-tests (msg &rest args)
  (let ((x (gensym))
	(y (gensym)))
    `(progn
       (format t "~&~a." ,msg)
       ,@(loop
	     with a = args
	     while a
		
	     collect `(let ((,x ,(first a))
			    (,y ,(second a)))
			
			(unless (equalp ,x ,y)
			  (setf *lhs* ,x
				*rhs* ,y)
			  (assert () ()
			    "~a equalled ~a instead of ~a"
			    ',(first a) ,x ,(second a)))
			(format t ".")
			)
	     do (setf a (cddr a)))
       (format t "check"))))

(defmacro do-boolean-tests (msg &rest args)
  `(do-tests ,msg
     ,@(loop
	   with a = args
	   while a
	   collect `(to-boolean ,(first a))
	   collect (second a)
	   do (setf a (cddr a)))))

(defmacro do-rand-tests (msg &rest args)
  (let ((x (gensym)))
    `(progn
       (format t "~&~a." ,msg)
       ,@(loop
	     with a = args
	     while a
		
	     collect `(let ((,x ,(first a)))
			
			(unless ,x
			  (setf *lhs* ,x)
			  (warn "~a did not succeed (it's a randomized test, so it may be due to chance)"
				',(first a)))
			(format t ".")
			)
	     do (setf a (cdr a)))
       (format t "check"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; generalized iteration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro do-iterator (iter (var &optional result-form num-var) &body body)
  "macro do-iterator ITERATOR (VAR &optional RESULT-FORM NUM-VAR) &body BODY

ITERATOR - a function (evaluated)
VAR - a symbol (not evaluated)
RESULT-FORM - a form (not evaluated)
NUM-VAR a symbol (not evaluated)
BODY - a list of forms enclosed by an implicit progn

A macro for writing other iteration macros.  Repeatedly calls the function ITERATOR, which must be a function that returns two values.  If the second value is nil, stop and return RESULT-FORM.  Otherwise, evaluate BODY with VAR bound to the first return value of the call to ITERATOR.  If NUM-VAR is provided, then during BODY it is bound to an integer representing the number of times (not including this one) the BODY has been evaluated so far."
  (with-gensyms (done iterator)
    `(loop
	 with (,var ,done)
	 with ,iterator = ,iter
         ,@(awhen num-var `(for ,num-var from 0))
	 do (multiple-value-setq (,var ,done)
	      (funcall ,iterator))
	 when ,done do (return ,result-form)
	 do ,@body)))

(defun map-iterator-to-list (fn iter)
  "map-iterator-to-list FUNCTION ITER.  ITER is a function that returns two values - NEXT-ITEM and DONE.  Until DONE returns true, repeatedly apply FUNCTION to the ITEMs and collect the results into a list."
  (loop
      with (next done)
      do (multiple-value-setq (next done)
	   (funcall iter))
      until done
      collect (funcall fn next)))

