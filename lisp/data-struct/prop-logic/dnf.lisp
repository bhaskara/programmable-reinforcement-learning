(in-package prop-logic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dnf
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun is-dnf-clause (x)
  "is-dnf-clause X. Is X a conjunction of literals or t or nil?"
  (or (is-constant x)
      (is-literal x)
      (and (is-compound-formula x)
	   (eql (compound-formula-type x) 'and)
	   (every #'is-literal (conjuncts x)))))

(defun is-dnf-formula (x)
  "is-dnf-formula X.  Is X a DNF formula, i.e. disjunction of DNF clauses?"
  (or (is-dnf-clause x)
      (and (is-disjunction x)
	   (every #'is-dnf-clause (disjuncts x)))))

(defun dnf-clauses (f)
  "dnf-clauses F - Return a list of clauses in DNF Formula F.  Does not check that F is a DNF formula."
  (if (is-dnf-clause f)
      (list f)
    (disjuncts f)))

(defun clause-literals (c)
  "clause-literals C.  Return list of literals in clause C.  If C is t, returns nil.  Asserts if C is nil."
  (assert c ())
  (if (eq c t)
      nil
    (if (is-literal c)
	(list c)
      (conjuncts c))))

(defun make-state-dnf (state props)
  "make-state-dnf STATE PROPS.  Make a DNF formula (clause, actually) that is true only for the assignment represented by STATE.  For this to be well-defined (since STATE is specified using the closed-world assumption), we also need to say what the overall set of proposition symbols is."
  (conjoin-list
	 (mapset 'list
		 (lambda (p)
		   (if (member p state)
		       p
		     (negate p)))
		 props)))


(defun simplify-dnf (f)
  "simplify-dnf DNF-FORMULA.  Check if FORMULA is a DNF.  Then simplify it, by first simplifying all the clauses, then dealing with trivial clauses and removing the initial or if necessary."
  (assert (is-dnf-formula f) () "~a is not a DNF formula" f)
  (let ((clauses (mapcar #'simplify-dnf-clause (dnf-clauses f))))
    (inter-clause-simplify clauses)))


(defun inter-clause-simplify (clauses)
  "inter-clause-simplify CLAUSES.  CLAUSES is a list of clauses.  Return a DNF formula that is equivalent to the disjunction of the clauses, with t's and nil's removed"
  (setf clauses (remove nil clauses))
  (if (member t clauses)
      t
    (if (<= (length clauses) 1)
	(first clauses)
      (disjoin-list clauses))))

(defun simplify-dnf-clause (f)
  "simplify-dnf-clause DNF-CLAUSE"
  (if (or (is-constant f) (is-literal f))
      f
    (let ((lits nil))
      (dolist (l (conjuncts f))
	(when (eql l nil)
	  (return-from simplify-dnf-clause nil))
	(unless (or (member l lits :test #'equal) (eql l t))
	  (if (member (negate l) lits :test #'equal)
	      (return-from simplify-dnf-clause nil)
	    (push l lits))))
      (case (length lits)
	(0 t)
	(1 (first lits))
	(otherwise (conjoin-list lits))))))
    


(defun literal< (l1 l2)
  (let ((p1 (literal-prop l1))
	(p2 (literal-prop l2)))
    (if (symbol< p1 p2)
	t
      (if (symbol< p2 p1)
	  nil
	(and (not (is-negation l1)) (is-negation l2))))))


(defun standardize-clause (c)
  "standardize-clause C.  Convert CLAUSE into a canonical equivalent representation."
  (if (or (is-constant c) (is-literal c))
      c
    (conjoin-list (sort (rest c) #'literal<))))

(defun clause< (c1 c2)
  (loop
      with l1 = (clause-literals c1)
      with l2 = (clause-literals c2)
		  
      for s1 = (first l1)
      for s2 = (first l2)
		  
      when (null l1)
      return t
	       
      when (null l2)
      return nil
	       
      when (literal< s1 s2)
      return t
	       
      when (literal< s2 s1)
      return nil
	       
      do (setf l1 (cdr l1)
	       l2 (cdr l2))
      finally (return nil)))
	       
		 
      

(defun standardize-dnf (f)
  "standardize-dnf FORMULA.  Convert FORMULA into a canonical equivalent simplified representation."
  (let* ((f2 (sort-dnf (simplify-dnf f) nil))
	 (unique-disjuncts
	  (if (is-disjunction f2)
	      (remove-duplicates (disjuncts f2) :test #'equalp)
	    (list f2))))
    (if (> (length unique-disjuncts) 1)
	(disjoin-list unique-disjuncts)
      (first unique-disjuncts))))

(defun sort-dnf (f &optional (clone-first t))
  "sort-dnf FORMULA &optional (NONDESTRUCTIVE t).  Assumes FORMULA has already been simplified, and converts it into a canonical representation."
  (when clone-first
    (setf f (clone f)))
  (if (is-dnf-clause f)
      (standardize-clause f)
    (disjoin-list (sort (mapcar #'standardize-clause (disjuncts f))
			   #'clause<))))


(defun implies (f1 f2)
  "implies F1 F2.  Logical implication.  Only works right now for F1 being a DNF formula and F2 a DNF clause."
  (flet ((implies-clause (c)
	   (let ((c1 (conjuncts c)))
	     (every
	      (lambda (conjunct)
		(member conjunct c1 :test #'equalp))
	      (conjuncts f2)))))
    
    (assert (and (is-dnf-formula f1) (is-dnf-clause f2)))
    (case f2
      ((t nil) f2)
      (otherwise (every #'implies-clause (dnf-clauses f1))))))



(defun clause-and (c1 c2)
  "clause-and C1 C2.  The logical and of the two clauses, simplified."
  (when (and c1 c2)
    (simplify-dnf-clause 
     (conjoin-list (append (clause-literals c1) (clause-literals c2))))))




(defun dnf-or (&rest f)
  "dnf-or &rest DNF-FORMULAE.  The (simplified) logical or of the two formulae."
  (simplify-dnf (disjoin-list (mapcan (lambda (x) (copy-list (dnf-clauses x))) f))))

(defun dnf-and (&rest f)
  "dnf-and &rest DNF-FORMULAE.  The simplified logical and of the formulae.  Repeatedly uses the 'distributive law'."
  (case (length f)
    (0 t)
    (1 (car f))
    (otherwise (dnf-binary-and (car f) (apply #'dnf-and (cdr f))))))
  
(defun dnf-binary-and (f1 f2)  
  "dnf-binary-and F1 F2.  The (simplified) logical and of the two formulae.  Uses the 'distributive law'."
  (let ((clauses2 (dnf-clauses f2)))
    (inter-clause-simplify 
     (mapcan
      (lambda (c1)
	(copy-list
	 (mapcar 
	  (lambda (c2) (simplify-dnf-clause (clause-and c1 c2)))
	  clauses2)))
      (dnf-clauses f1)))))

(defun pprint-dnf-clause (str clause)
  (pprint-logical-block (str (if (is-conjunction clause) (conjuncts clause) clause) :prefix "(" :suffix ")")
    (loop
      (pprint-exit-if-list-exhausted)
      (let ((conjunct (pprint-pop)))
	(pprint-logical-block (str nil)
	  (pprint-pop)
	  (if (is-prop-symbol conjunct)
	      (write conjunct :stream str)
	    (format str "(not ~a)" (second conjunct)))
	  (pprint-newline :fill str)))
      
      (pprint-exit-if-list-exhausted)
      (princ " and " str)
      (pprint-newline :fill str))))

(defun pprint-dnf (str f)
  (let ((clauses (dnf-clauses f)))
    (if (= 0 (length clauses))
	(princ nil str)
      (pprint-logical-block (str (dnf-clauses f) :prefix "(" :suffix ")")
	(loop
	  (pprint-exit-if-list-exhausted)
	  (pprint-dnf-clause str (pprint-pop))
	  (pprint-exit-if-list-exhausted)
	  (princ " or " str)
	  (pprint-newline :fill str))))))

(set-pprint-dispatch '(and list (not null) (satisfies is-dnf-formula)) #'pprint-dnf 0)
(set-pprint-dispatch '(and list (not null) (satisfies is-dnf-clause)) #'pprint-dnf-clause 1)
(set-pprint-dispatch '(and list (not null) (satisfies is-literal)) (pprint-dispatch '(a)) 2)