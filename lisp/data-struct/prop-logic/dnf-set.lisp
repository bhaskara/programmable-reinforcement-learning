;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lookahead/prop/dnf-set.lisp
;; Represents the set of satisfying assignments for a DNF formula
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package prop-logic)

(defclass <dnf-set> (<set>)
  ((formula :type [formula]
	    :reader formula
	    :initarg :formula
	    :writer set-formula)
   (propositions :type [numbered-set]
		 :reader props
		 :initarg :props
		 :writer set-props))
  (:documentation "<dnf-set>.  Represents the set of satisfying assignments of a DNF formula. Create using make-dnf-set.

DNF sets support many of the standard set operations.  One thing to note is that the operation same is not exactly correct - it returns t iff the two formulae have the same canonical representations.  So it might sometimes fail to detect equivalent formulae (not sure)."))

(defun make-dnf-set (formula props)
  "make-dnf-set FORMULA PROPS.  Make a <dnf-set> object representing the set of satisfying assignments to FORMULA over the symbols PROPS (which must include those in formula)."
  (let ((formula-props (prop-symbols formula)))
    (assert (subset formula-props props) ()
      "Symbols in ~a are ~a, which is not a subset of ~a"
      formula formula-props props)
    (assert (is-dnf-formula formula) ()
      "~a is not a DNF formula" formula)
    (make-instance '<dnf-set> :formula (standardize-dnf formula) :props props)))


(defmethod member? (item (s <dnf-set>))
  (let ((props (props s)))
    (and
     (listp item)
     (every (lambda (x) (member? (literal-prop x) props)) item)
     (holds item (formula s)))))

(defmethod intersect ((s <dnf-set>) (s2 <dnf-set>))
  (let ((p (props s))
	(p2 (props s2)))
    (assert (set-eq p p2) ()
      "Sets ~a and ~a have differing proposition sets ~a and ~a" 
      s s2 p p2)
    (make-dnf-set
     (dnf-and (formula s) (formula s2))
     p)))


(defmethod binary-union ((s <dnf-set>) (s2 <dnf-set>))
  (let ((p (props s))
	(p2 (props s2)))
    (assert (set-eq p p2) ()
      "Sets ~a and ~a have differing proposition sets ~a and ~a" 
      s s2 p p2)
    (make-dnf-set
     (dnf-or (formula s) (formula s2))
     p)))

(defmethod subset ((s <dnf-set>) (s2 <dnf-set>))
  (implies (formula s) (formula s2)))

(defmethod clone ((s <dnf-set>))
  (make-dnf-set
   (clone (formula s))
   (clone (props s))))

(defmethod print-object ((s <dnf-set>) str)
  (format str "<<DNF Set ")
  (pprint-dnf str (formula s))
  (format str ">>"))

(defmethod same ((s1 <dnf-set>) (s2 <dnf-set>))
  (equalp (formula s1) (formula s2)))