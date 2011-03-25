(defpackage set
  (:documentation "Package for operations related to sets.

Set types, creation
- [set]
- [numbered-set]
- <set>
- <numbered-set>
- <indexed-set>
- <directory-set>
- recursive-closure
- powerset
- make-image-set
also, see the prod-set package

Symbols naming sets
- natural-numbers
- real-numbers
- universal-set

Exported Operations
- member? X S
- iterator S
- size S 
- is-empty
- item-number
- item
- do-elements
- same 
- add
- addf
- unionf
- mapset
- filter
- sum-over
- subset
- intersect
- binary-union
- unite
- set-eq
- symmetric-difference

Operations for children
- iterator-done
- iterator-not-done


Conditions
- item-not-in-set
- index-out-of-bounds

Operations on conditions
- get-explanation-string
- get-full-explanation-string

Related packages
- prod-set

The operations for sets are assumed to use #'same for equality testing.

")
  (:use common-lisp
	utils)
  (:export [set]
	   [numbered-set]
	   <set>
	   <numbered-set>
	   <indexed-set>
	   <directory-set>
	   make-image-set
	   natural-numbers
	   real-numbers
	   universal-set
	   member?
	   iterator
	   iterator-not-done
	   iterator-done
	   item-number
	   item
	   do-elements
	   mapset
	   filter
	   sum-over
	   subset
	   intersect
	   binary-union
	   unite
	   set-eq
	   symmetric-difference
	   same
	   add
	   addf
	   unionf
	   recursive-closure
	   powerset
	   size
	   is-empty
	   item-not-in-set
	   index-out-of-bounds
	   get-explanation-string
	   get-full-explanation-string
	   ))

(in-package set)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type defs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defclass <set> ()
  ()
  (:documentation "Abstract class for sets."))

(deftype [set] ()
  "Type for sets.  Can be one of
1. A numbered set (see [numbered-set])
2. Object of type <set>
3. The symbol 'real-numbers
4. The symbol 'universal-set"
  `(or <set> [numbered-set] (eql 'real-numbers) (eql 'universal-set)))


(defclass <numbered-set> (<set>)
  ()
  (:documentation "Abstract class for numbered sets."))

(deftype [numbered-set] ()
  "Type for numbered sets, i.e., sets in which the elements are numbered, starting from 0.  Iterating over a numbered sets always returns the elements in increasing order of number.  Can be one of
1. A sequence, signifying the set of its elements
2. A fixnum n, signifying the set {0,1,...,n-1}
3. A hashtable from items to numbers
4. 'natural-numbers, signifying the set {0,1,2,...}
5. Object of type <numbered-set>"
  `(or sequence fixnum hash-table <numbered-set> (eql 'natural-numbers)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; conditions and related types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition item-not-in-set ()
  ((item :initarg :item :reader get-item)
   (s :initarg :set :reader get-set))
  (:report (lambda (c s) (format s "~a not in set ~a" (get-item c) (get-set c))))
  (:documentation "Objects of type item-not-in-set play two roles.  First, when calling the function member?, they are returned as a secondary value that denotes the reason why an item is not a member of a set.  Second, certain functions (e.g., item-not-in-set) require certain set membership relations to hold.  When they do not, those functions may signal a condition of type item-not-in-set. 

Subclasses should implement get-explanation-string."))

(define-condition index-out-of-bounds ()
  ((ind :reader ind :initarg :ind)
   (s :initarg :set :reader get-set)
   (max-ind :reader max-ind :initarg :max-ind))
  (:report (lambda (c s) (format s "Index ~a out of bounds for set ~a (max allowed is ~a)" (ind c) (get-set c) (max-ind c))))
  (:documentation "Signalled when trying to find an element with a nonexistent index in a [numbered-set]."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; generic functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric member? (item s)
  (:documentation "member? ITEM SET.  Return T iff ITEM or something that is the #'same as it belongs to SET.  If returning nil, may return a second value, which is an object of type item-not-in-set that denotes the reason ITEM is not a member of SET.  There is currently an :around method at the top-level which, if the item is not a member and the reason is nil, makes an item-not-in-set object that includes the item and the set (this is a temporary solution - eventually, the second return value should be mandatory).")
  (:method :around (item s)
	   (multiple-value-bind (present? reason)
	       (call-next-method)
	     (values present?
		     (if (or present? reason)
			 reason
		       ;; if not present, and a reason is not provided, make a default one
		       ;; TODO this is a bit hacky
		       (make-instance 'item-not-in-set :item item :set s))))))
	       
	       

(defgeneric iterator (s)
  (:documentation "iterator SET.  Returns a function F.  Calling F returns 1) the next item of F, if it exists, or an arbitrary value otherwise 2) T if there were no more elements.  The first return value only makes sense if the second one is NIL.  A standard way to use iterator is via do-elements or mapset.  There is a default method for iterator that just uses item-number"))

(defgeneric item-number (item s)
  (:documentation "item-number ITEM NUMBERED-SET.  Return the number of item in the set.  Signals an 'item-not-in-set error if item is not found."))

(defgeneric item (num s)
  (:documentation "item NUM NUMBERED-SET. Return the item with this number.  An around method for the abstract class ensures thatthe number is a valid index for this set, or signals an 'index-out-of-bounds error otherwise.")
  (:method :around (num s)
	   (if (between2 num 0 (size s))
	       (call-next-method)
	     (error 'index-out-of-bounds :set s :ind num :max-ind (1- (size s))))))

(defgeneric size (s)
  (:documentation "size SET.  Return the number of items in the set if it's finite.  TODO figure out what to do when it's infinite.")
  (:method ((s t)) 'undefined))

(defgeneric is-empty (s)
  (:method ((s t)) (eql (size s) 0)))
	   

(defgeneric add (s item &optional pos)
  (:documentation "add S ITEM &optional (POS nil).  If ITEM doesn't exist in the set S, add it and return the new set.  If not, return the original.  Might destructively modify the original set.  If POS is nil, then the item numbers in the set may be changed arbitrarily.  If POS is t, then none of the existing item numbers are changed, and if the item is being added newly, it will be added to the end (i.e. with the highest item number).  If this is not possible (e.g. with a sorted set representation), an assert should happen."))


(defgeneric get-explanation-string (c)
  (:documentation "get-explanation-string ITEM-NOT-IN-SET-CONDITION.  Return a string that explains verbally why an item is not in a set.  It should be assumed to come after a string of the form 'X was not a member of S, the reason being ' and followed by a period.  See also get-full-explanation-string.")
  (:method (c)
	   (if (symbolp (get-item c))
	       "unspecified (the item is a symbol.  If a symbol with the same name appears to be in the set, then perhaps there's a package issue)"
	     "unspecified")))

(defun get-full-explanation-string (c)
  "get-full-explanation-string ITEM-NOT-IN-SET-CONDITION.  Return a string that explains why an item is not in a set.  The string will be a full sentence of the form 'X was not a member of S, the reason being ...'.  Calls get-explanation-string."
  (format nil "~a was not a member of ~a.~%The reason is that ~a."
	  (get-item c) (get-set c) (get-explanation-string c))) 
  



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; operations for use by children
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro iterator-not-done (item)
  `(values ,item nil))

(defmacro iterator-done ()
  `(values nil t))
	    


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; operations defined here for external use
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    

(defmacro do-elements ((var s &optional result-form num-var) &body body)
  "macro do-elements (VAR SET &optional RESULT-FORM NUM-VAR) &body BODY.  Loop over the elements of a set in order.  During BODY, VAR is bound to successive elements of the set.  If NUM-VAR is provided, then during BODY, it is bound to the index of the corresponding element.  If RESULT-FORM is provided, this is the return value.  Otherwise, return NIL."
  `(do-iterator (iterator ,s) (,var ,result-form ,num-var) ,@body))


(defgeneric mapset (result-type fn s)
  (:documentation "mapset RESULT-TYPE FN SET.  Apply FN to each element of SET.  If RESULT-TYPE is 'list, return the results in a list, and if its 'vector, return the results in a vector. If its a list of the form '(vector type), return the results in a vector where the elements are of that type.  In the latter two cases, the vector is guaranteed to be simple."))

(defmethod mapset ((result-type (eql 'list)) fn s)
  (map-iterator-to-list fn (iterator s)))
		 
(defmethod mapset ((result-type (eql 'vector)) fn s)
  (let ((results (make-array (size s))))
    (do-elements (item s results i)
      (setf (svref results i) (funcall fn item)))))

(defmethod mapset ((result-type list) fn s)
  (assert (eq (first result-type) 'vector))
  (let ((results (make-array (size s) :element-type (second result-type))))
    (do-elements (item s results i)
      (setf (svref results i) (funcall fn item)))))
    

(defgeneric filter (result-type s pred)
  (:documentation "filter RESULT-TYPE SET PREDICATE.  Return a new set containing the elements of SET that satisfy PREDICATE.  RESULT must be 'list for now."))

(defmethod filter ((result-type (eql 'list)) s pred)
  (let ((l nil))
    (do-elements (x s l)
      (when (funcall pred x) (push x l)))))


(defgeneric sum-over (s fn)
  (:documentation "sum-over FUNCTION SET.  Repeatedly apply FUNCTION to each member of SET and sum the results.")
  (:method (s fn)
	   (let ((total 0))
	     (do-elements (x s total)
	       (incf total (funcall fn x)))))
  (:method ((s sequence) fn)
	   (reduce #'+ s :key fn)))


  
  

(defgeneric subset (s1 s2)
  (:documentation "subset SET1 SET2.  Is SET1 a subset of SET2?")
  (:method (s1 s2)
	   (do-elements (x s1 t)
	     (unless (member? x s2)
	       (return-from subset nil)))))
	   

(defmethod iterator ((s t))
  (check-type s [numbered-set])
  (let ((ind 0)
	(size (size s)))
    (lambda ()
      (if (< ind size)
	  (let ((item (item ind s)))
	    (incf ind)
	    (iterator-not-done item))
	(iterator-done)))))

	  

(defmacro addf (s item &optional (pos nil))
  "Macro addf SET ITEM &optional (POS nil).  Add ITEM to SET if it doesn't already exist."
  `(_f add ,s ,item ,pos))

(defgeneric intersect (s1 s2)
  (:documentation "intersect S1 S2.  Return a new set that is the intersection of S1 and S2."))

(defgeneric binary-union (s1 s2)
  (:documentation "binary-union S1 S2.  Return a new set that is the union of S1 and S2."))

(defgeneric destructive-union (s s2)
  (:documentation "destructive-union S1 S2.  Add the elements of S2 to S1.  There is a default method that works for all sets by repeatedly calling addf.")
  (:method (s s2)
	   (do-elements (x s2 s)
	     (addf s x))))
  

(defmacro unionf (s1 s2)
  "Macro unionf S1 S2.  Set S1 to the union of S1 and S2."
  `(_f destructive-union ,s1 ,s2))

(defun unite (sets)
  "unite SETS.

SETS is a list of sets.
  
Nondestructive operation that returns the union of the sets."
  (let ((l nil))
    (dolist (s sets l)
      (do-elements (x s)
	(adjoinf l x :test #'same)))))


(defgeneric set-eq (s1 s2)
  (:documentation "set-eq S1 S2.  Are S1 and S2 equal as sets - i.e. is there a bijective mapping between them that commutes with #'same?")
  (:method (s1 s2)
	   (and (eq (size s1) (size s2))
		(do-elements (x s1 t)
		  (unless (member? x s2)
		    (return nil))))))



(defgeneric symmetric-difference (s1 s2)
  (:documentation "symmetric-difference S1 S2.  Return 2 values : 1) the set of elements of S1 not in S2. 2) The set of elements of S2 not in S1.")
  (:method (s1 s2)
	   (values
	    (filter 'list s1 (lambda (x) (not (member? x s2))))
	    (filter 'list s2 (lambda (x) (not (member? x s1)))))))
	   




(defun argext (s identity-element op key-fn)
  "argext SET IDENTITY-ELT OP KEY-FUNCTION
primitive operation for operations like argmax and argmin.  Should not be called by external code.

SET - a set
IDENTITY-ELT - identity element of operation
OP - a function of two arguments
KEY-FN - how to get a value from each element of the set (usually #'identity)

Let E be the first element with the extreme value of key (i.e. its value VAL is such such that (op VAL OTHER-VAL) is true for all values OTHER-VAL of elements in the set).
argext returns
1) The index of E
2) The key value
3) The element
4) List of all indices having this value"


  (let ((best nil)
	(best-val identity-element)
	(all-best nil)
	(best-elt nil))
    (flet ((new-best (i val e)
	     (setf best i
		   best-val val
		   best-elt e
		   all-best (list i))))

      (do-elements (x s (values best best-val best-elt all-best) i)
	(let ((val (funcall key-fn x)))
	  (if best
	      (cond
	       ((eql val best-val) (push i all-best))
	       ((funcall op val best-val) (new-best i val x)))
	    (new-best i val x)))))))

(defun argmax (s &key (key #'identity))
  "argmax NUMBERED-SET &key (KEY #'identity).  Return four values : 1) index of first elt with max key val 2) the val 3) the first element achieving this max 4) list of all indices with this val.  The keys may be real numbers, '-infty, or '-infty."
  (argext s '-infty #'my> key))

(defun argmin (s &key (key #'identity))
  "argmin NUMBERED-SET &key (KEY #'identity).  Return three values : 1) index of first elt with min key val 2) the key val 3) the first element achieving this min 4) list of all indices with this val.  The keys may be real numbers, '-infty, or '-infty."
  (argext s 'infty #'my< key))



(in-package common-lisp-user)
	







