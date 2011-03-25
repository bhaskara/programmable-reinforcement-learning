(defpackage direct-product-set
  (:documentation "Package direct-product-set (prod-set)

A <prod-set> is the direct product of a finite number of finite [numbered-set]s. 

Types and constructors
----------------------
<prod-set>
<var-set>

Operations
----------
sets - the sets of values of each variable
inst-acc - the instantiation accessor for elements of the set
make-subspace - create the subspace corresponding to some subset of the variables.

")
  (:nicknames prod-set)
  (:export 
   <prod-set>
   <var-set>
   sets
   uninstantiated
   not-used
   inst-acc
   make-subspace)
  (:use cl
	utils
	inst-vars
	set))

(in-package prod-set)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Class definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <prod-set> (<numbered-set>)
  ((sets :reader sets :writer set-sets :type vector)
   (dim :writer set-dim :reader dim :type fixnum)
   (size :writer set-size :reader size :initform 'unknown)
   (sizes :reader sizes :writer set-sizes :type vector)
   (inst-accessor :writer set-inst-acc :reader inst-acc :initarg :inst-acc)
   (iterate-quickly :initarg :iterate-quickly :initform nil :reader iterate-quickly)
   (size-products :writer set-size-prods :reader size-prods :type (simple-array * 1)
		  :documentation "For example, if the successive dimensions have cardinality 3, 4, 2, 5 then this would be the array #(40 10 5 1)")
)
  (:documentation "A subclass of set for sets of instantiations to a finite set of variables, each of which can take a finite set of values.  Uses time, memory proportional to the dimension, not the actual size (i.e. doesn't actually store all the possible joint values).

Required initargs
:sets - a sequence of sets.  Variable I ranges over the Ith set in this sequence.

:inst-acc - an instantiation accessor (see inst-vars package) used to collect together variable values into an object.  Must be specified if creating an instance of this class (but subclasses may provide it automatically, e.g <var-set>).

If :inst-acc is not provided, the following argument must be provided
:alist-keys - a list of keys, whose length equals the number of sets.  Elements are then association lists from these keys.
If :alist-keys is provided
:alist-test may also be provided.  Defaults to #'eql.

Optional
:iterate-quickly - nil by default.  If t, then iteration operations (iterator, do-elements, mapset) will work in a more efficient way by not creating a new object each time. This will work ok so long as the calling code does not modify the returned objects, or expect their values to persist (e.g. by saving them in a lexical closure).

It is an error for any of the values in the sets to be the symbol 'uninstantiated
"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constructor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((ps <prod-set>) &rest args 
				       &key sets alist-keys (alist-test #'eql) inst-acc)
  (when alist-keys
    (assert (not inst-acc))
    (set-inst-acc (make-alist-accessors alist-keys alist-test) ps))
  (let ((sizes (map 'vector #'size sets)))
    (set-sizes sizes ps)
    (when (every #'numberp sizes)
      (let ((set-vec (coerce sets 'vector))
	    (size-prods 
	     (reverse
	      (let ((cumulative-prod 1))
		(map 'vector
		  (lambda (x)
		    (prog1 
			cumulative-prod
		      (_f * cumulative-prod x)))
		  (reverse sizes))))))
    
	(set-sets set-vec ps)
	(let ((dim (length sizes)))
	  (set-dim dim ps)
	  (set-size 
	   (if (> dim 0)
	       (* (aref sizes 0) (aref size-prods 0))
	     0)
	   ps))
	(set-size-prods size-prods ps)))))


(defmethod clone ((s <prod-set>))
  (make-instance '<prod-set> :sets (sets s)
		 :inst-acc (inst-acc s)))

(defmethod print-object ((s <prod-set>) str)
  (format str "<<Direct product of sets ~a>>" (sets s)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; subtypes of item-not-in-set
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <invalid-inst> (item-not-in-set)
  ((reason :initarg :reason :reader reason))
  (:documentation "Class <invalid-inst> (item-not-in-set)
Represents an explanation for an item not to be in a product set because of it not being a well-formed instantiation of the sort required by that product set."))

(defmethod get-explanation-string ((c <invalid-inst>))
  (format nil "the instantiation is invalid for reason ~a" (reason c)))


(defclass <invalid-component> (item-not-in-set)
  ((comp :initarg :comp :reader comp)
   (comp-set :initarg :comp-set :reader comp-set)
   (reason :initarg :reason :reader reason))
  (:documentation "Class <invalid-component> (item-not-in-set)
Represents an explanation for an item not to be in a product set because one of its components is not a member of the corresponding component set."))

(defmethod get-explanation-string ((c <invalid-component>))
  (format nil "component ~a is not a member of set ~a, the reason for *that* being ~a"
	  (comp c) (comp-set c) (get-explanation-string (reason c))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; accessing instantiations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (inline get-comp set-comp make-empty-element))

(defun get-comp (ps x i)
  "get-comp PRODUCT-SET INSTANTIATION VAR-NUM."
  (get-var-val (inst-acc ps) x i))

(defun set-comp (ps x i val)
  "set-comp PRODUCT-SET INSTANTIATION VAR-NUM VALUE"
  (set-var-val (inst-acc ps) x i val))

(defun make-blank-inst (ps)
  "make-blank-inst PRODUCT-SET"
  (create-inst (inst-acc ps)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; operations from set
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod member? (x (ps <prod-set>))
  (flet ((not-member (reason-class &rest reason-args)
	   (return-from member?
	     (values nil (apply #'make-instance reason-class reason-args)))))
    (aif (detect-invalid-inst (inst-acc ps) x)
	 (not-member '<invalid-inst>
		     :item x :set ps :reason it)
	 (loop
	     for i below (dim ps)
	     for s across (sets ps)
	     for comp = (get-comp ps x i)

	     do (multiple-value-bind (comp-is-member? reason)
		    (member? comp s)
		  (unless comp-is-member?
		    (not-member '<invalid-component>
				:item x :set ps
				:comp comp :comp-set s
				:reason reason)))
	     finally (return t)))))
		
			

(defmethod item-number (x (ps <prod-set>))
  (loop
      for i below (dim ps)
      for prod across (size-prods ps)
      for s across (sets ps)
      for y = (get-comp ps x i)
		   
      sum (* prod (item-number y s))))

(defmethod item (num (ps <prod-set>))
  ;; TODO signal condition when num out of bounds
  (let ((y num)
	(x (make-blank-inst ps)))
    (loop
	for i below (dim ps)
	for s across (sets ps)
	for p across (size-prods ps)
	do (multiple-value-bind (q r)
	       (floor y p)
	     (setf y r)
	     (set-comp ps x i (item q s)))
	finally (return x))))


(defmethod iterator ((s <prod-set>))
  (if (iterate-quickly s)
      
      ;; if iterating quickly, do a more efficient version that uses the same
      ;; object each time
      (let ((sizes (sizes s))
	    (dim (dim s))
	    (sets (sets s))
	    (total-size (size s))
	    (acc (inst-acc s)))
	(assert (every #'numberp sizes) ()
	  "Iterator cannot be defined because the coordinate sizes ~a of ~a are not all numbers."
	  sizes s)
	
	(if (> total-size 0)
	    
	    ;; if set is nonempty
	    (let ((current-ind 0)
		  (inst (item 0 s))
		  (iterators (map 'vector #'iterator sets)))
	  
	      ;; start each iterator off
	      (map nil #'funcall iterators)

	  
	      (if (some #'zerop sizes)
		  ;; if the set is empty, the iterator is done immediately
		  (lambda ()
		    (iterator-done))
	    
		;; Otherwise
		(lambda ()
	      
		  ;; the prog1 is to step current-ind each time
		  (multiple-value-prog1
		  
		      ;; deal with init and finish cases
		      (if (eql current-ind 0)
			  (iterator-not-done inst)
			(if (>= current-ind total-size)
			    (iterator-done)
		      
			  ;; main case
			  (loop
			
			  ;;; start at end and work way back
			      for i from (1- dim) downto 0
						   
			      do (multiple-value-bind (val done?)
				     (funcall (aref iterators i))
			       
			       ;;; if this iterator cannot be stepped, reset this coord to beginning 
				   (if done?
				       (let ((iter (iterator (aref sets i))))
					 (setf (aref iterators i) iter)
					 (multiple-value-bind (val done?)
					     (funcall iter)
					   (assert (not done?) () "iterator unexpectedly finished for set ~a"
						   (aref sets i))
					   (set-var-val acc inst i val)))
				 
				     ;; otherwise, step this iterator and end loop
				     (return 
				       (progn
					 (set-var-val acc inst i val)
					 (iterator-not-done inst)))))
			     
			      finally (assert () () "Iteration error for prod set ~a, current item ~a" s inst))))
		    (incf current-ind)))))
	
	  ;; if set is empty
	  (lambda () (values nil t))))
  
    ;; if the iterate-quickly flag is off, just use the default method of set
    (call-next-method)))
    
  
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; subspaces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun make-subspace (s vars &optional (iterate-quickly nil))
  "make-subspace S VAR-NAMES &optional (ITERATE-QUICKLY nil)

S is a product set.
VAR-NAMES is a list of variables names.

Create a representation of the set of instantiations to the variables in VARS.  The elements of this set will actually be full joint instantiations from S, but the variables not in VARS will be ignored, or have their value set to 'not-used.

ITERATE-QUICKLY has the same meaning as when creating new <prod-set> objects."
  (let* ((sets (sets s))
	 (acc (inst-acc s))
	 (names (var-names acc))
	 (var-nums (mapcar (lambda (n) 
			     (item-number n names))
			   vars)))
      (make-instance '<prod-set>
	:sets (map 'vector (lambda (v) (aref sets v)) var-nums)
	:inst-acc (make-subinst-accessors (inst-acc s) vars)
	:iterate-quickly iterate-quickly)))


(defun make-subinst-accessors (inst-acc vars)
  "make-subinst-accessors INST-ACC VARS.  
INST-ACC - instantiation accessor
VARS - list of variable ids

Given an accessor for overall joint instantiations, we want to make a sub-instantiation accessor for a subset of the variables that can take in instantiations to the entire set of variables, but treat them as if they were projected down to the subset."
  
  (let ((n (num-vars inst-acc))
	(index (map 'vector #'(lambda (v) (get-var-num inst-acc v)) vars)))
    (let ((creator #'(lambda () 
		     (let ((a (create-inst inst-acc)))
		       (dotimes (i n)
			 (set-var-val inst-acc a i 'not-used))
		       (map nil 
			 #'(lambda (j) 
			     (set-var-val inst-acc a j 'uninstantiated))
			 index)
		       a)))
	  (readers (map 'vector
		     #'(lambda (j)
			 #'(lambda (a) 
			     (get-var-val inst-acc a j)))
		     index))
	  (writers (map 'vector
		     #'(lambda (j)
			 #'(lambda (a val)
			     (set-var-val inst-acc a j val)))
		     index))
	  (invalid-inst-detector 
	   #'(lambda (i) 
	       (or (detect-invalid-inst inst-acc i)
		   (dotimes (j n)
		     (unless (member? j index)
		       (let ((val (get-var-val inst-acc i j)))
			 (unless (eq val 'not-used)
			   (return (list 'unused-variable-has-value j val)))))))))
		   
	  (var-num (lambda (v) (position v vars :test #'equal))))
      
      ;; TODO need to specify the invalid inst detector
      (make-inst-var-accessors
       :creator creator
       :readers readers
       :writers writers
       :var-num var-num
       :invalid-inst-detector invalid-inst-detector
       :var-names vars))))
			   
	 



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; internal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Cumulative product of list elements
;; i.e. given a list (n1 n2 .. nk) returns
;; (1 n1 n1*n2 ... n1*n2*...*nk)
(defun cumprod (l)
  (if (null l)
      '(1)
    (cons 1
	  (mapcar 
	   #'(lambda (x) (* x (first l)))
	   (cumprod (rest l))))))
	  
  
(in-package common-lisp-user)    











