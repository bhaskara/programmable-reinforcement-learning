(defpackage prop-logic
  (:documentation "Package for representing formulas in propositional logic.

Formulas are representing using the [formula] type.  Elementary propositions are represented by symbols, and t and nil represent the logical constants TRUE and FALSE.  A propositional state, using the closed world assumption, is represented by a list of propositions that are true in that state.

Basic creation and access
-------------------------
conjoin
conjoin-list
disjoin
disjoin-list
negate
conjuncts
disjuncts
negatee
literal-prop

Types
-----
is-state
[state]
is-formula
[formula]
is-literal
is-dnf-clause
is-dnf-formula
is-negation
is-conjunction
is-disjunction

General logic
-------------
holds
implies

DNF
---
standardize-dnf
sort-dnf
simplify-dnf
dnf-or
dnf-and
dnf-clauses

DNF sets
--------
<dnf-set>
make-dnf-set
formula
props

Debugging
---------
pprint-dnf

   ")
  

  (:export
   conjoin
   conjoin-list
   disjoin
   disjoin-list
   negate
   conjuncts
   disjuncts
   negatee
   literal-prop
   
   is-state
   [state]
   is-formula
   [formula]
   is-literal
   is-negation
   is-conjunction
   is-disjunction

   holds
   implies
   
   standardize-dnf
   sort-dnf
   simplify-dnf
   dnf-or
   dnf-and
   is-dnf-clause
   is-dnf-formula
   dnf-clauses
   
   make-dnf-set
   <dnf-set>
   formula
   props
   
   pprint-dnf
   )
  (:use
   cl
   utils
   set))
   

(in-package prop-logic)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic creation and access
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun conjoin (&rest args)
  "conjoin &rest FORMULAE.  Return a formula representing the logical and of the FORMULAE."
  (cons 'and args))

(defun conjoin-list (l)
  "conjoin-list L.  Return a formula representing the logical and of the formulae in list L."
  (cons 'and l))

(defun disjoin (&rest args)
  "disjoin &rest FORMULAE.  Return a formula representing the logical or of the FORMULAE."
  (cons 'or args))

(defun disjoin-list (l)
  "disjoin-list L.  Return a formula representing the logical or of the formulae in list L."
  (cons 'or l))

(defun negate (f)
  "negate FORMULA.  Return the negation of FORMULA, and also do simplification to remove double negatives."
  (if (symbolp f)
      (list 'not f)
    (negatee f)))

(defun negatee (x)
  "negatee X.  For a negation, return the thing that's being negated (answer undefined if not a negation)."
  (second x))

(defun conjuncts (x)
  "conjuncts X.  For a conjunction, return list of formulae being conjoined.  Not a fresh list, so copy it before modifying.  Answer undefined if not a conjunction."
  (if (symbolp x)
      (list x)
    (rest x)))

(defun disjuncts (x)
  "disjuncts X.  For a disjunction, return list of formulae being disjoined.  Not a fresh list, so copy it before modifying.  Answer undefined if not a disjunction."
  (if (symbolp x)
      (list x)
    (rest x)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; various basic types of formulae
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun is-constant (x)
  "is-constant X.  Is X one of t or nil?"
  (member x '(t nil)))

(defun is-prop-symbol (x)
  "is-prop-symbol X.  Is X a proposition symbol?"
  (and (symbolp x) (not (is-constant x))))

(defun is-compound-formula (x)
  "is-compound-formula X.  Is X an and, not, or or (may not recursively check all the arguments for well-formedness)?"
  (and (listp x)
       (member (car x) '(or not and))))

(defun is-conjunction (x)
  "is-conjunction X.  Is X an and (does not recursively check arguments for wellformedness)?"
  (and (listp x) (eq (car x) 'and)))

(defun is-disjunction (x)
  "Is X an or (does not recursively check arguments for wellformedness)?"
  (and (listp x) (eq (car x) 'or)))

(defun is-negation (x)
  "Is X a not (does not recursively check arguments for wellformedness)?"
  (and (listp x) (eq (car x) 'not) (eq (length x) 2) ))

(defun compound-formula-type (x)
  "compound-formula X.  For a compound-formula, return the type ('or, 'not, or 'and) (answer undefined if not a compound formula)."
  (car x))

(defun is-literal (x)
  "is-literal X.  Is X a proposition or negated proposition?"
  (or (is-prop-symbol x)
      (and (is-negation x)
	   (is-prop-symbol (negatee x)))))

(defun literal-prop (l)
  "literal-prop L.  Get the proposition used in L.  Answer undefined if not a literal."
  (if (symbolp l) l 
    (progn
      (assert (eq (first l) 'not))
      (second l))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general formulae
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun is-formula (x)
  "is-formula X

Returns true iff X is one of
1) t
2) nil
3) A symbol, denoting a proposition with that name
4) A list '(not F) where F is a formula
5) A list '(S . F) where F is a list of formulae and S is either 'and or 'or"
  (or
   (is-constant x)
   (is-prop-symbol x)
   (cond
    ((is-negation x) (is-formula (negatee x)))
    ((is-conjunction x) (every #'is-formula (conjuncts x)))
    ((is-disjunction x) (every #'is-formula (disjuncts x))))))

(deftype [formula] ()
  "Type for propositional formulae.  Well-formed formulae are determined using the predicate is-formula.  See its documentation for more."
  `(satisfies is-formula))


(defun prop-symbols (formula &optional (result-type 'list))
  "prop-symbols FORMULA &optional (RESULT-TYPE 'list).  Return the set of propositions symbols used in this FORMULA.  RESULT-TYPE can only be 'list for now."
  (assert (eql result-type 'list))
  (let ((l nil))
    (labels 
	((helper (f)
	   (cond
	    ((is-literal f)
	     (adjoin (literal-prop f) l))
	    ((is-compound-formula f)
	     (ecase (compound-formula-type f)
	       (not (helper (negatee f)))
	       (and (map nil #'helper (conjuncts f)))
	       (or (map nil #'helper (disjuncts f))))))
	   (values)))
      (helper formula))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; states
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun is-state (x)
  "is-state X.  Returns true if X is a list of symbols."
  (every #'symbolp x))

(deftype [state] ()
  "Type for propositional states, which must be lists of symbols."
  `(satisfies is-state))

		 
		 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; satisfaction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defgeneric holds (state formula)
  (:documentation "holds STATE FORMULA.  Does STATE make FORMULA true?  STATE is, for now, a list of the propositions that are true.")
  (:method (state (formula (eql t)))
	   (declare (ignore state))
	   t)
  (:method (state (formula (eql nil)))
	   (declare (ignore state))
	   nil)
  (:method (state (formula symbol))
	   (member formula state))
  (:method (state (formula list))
	   (ecase (first formula)
	     (and (every (lambda (f) (holds state f)) (rest formula)))
	     (or (some (lambda (f) (holds state f)) (rest formula)))
	     (not (not (holds state (second formula)))))))
	     

