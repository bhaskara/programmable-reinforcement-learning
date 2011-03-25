(defpackage probability
  (:documentation "A package for probability distributions and operations on them.  

Main types
----------
[prob-dist] - basic type for probability distributions.  
<prob-dist> - class that can be extended to create new probability distributions.
[cond-prob-dist] - basic type for conditional distributions
[random-variable] - basic type for random variables

Common distributions
----------------------
make-multinomial-dist
make-boltzmann-dist
make-unif-dist-over-set
sample-uniformly
make-deterministic-dist
standardize-alist-prob-dist
make-rv-dist 
make-invertible-rv-dist
make-mixture-dist

Some types of distributions
---------------------------
<rv-dist>
<sampling-without-replacement>
<sampling-with-replacement>


Basic operations on a probability distribution (which may not be implemented by all of them)
--------------------------------------------------------------------------------------------
sample P
prob P X
expectation P RV
sample-iterator
sample-space P
do-sample-points
is-valid-prob-dist
map-prob-dist
updatef
clone-dist

Operations on a conditional probability distribution
----------------------------------------------------
cond-dist P X
cond-prob P Y X
cond-sample P X
cond-exp P RV

Operations on a random variable
-------------------------------
evaluate-rv RV X


Conditions
----------
invalid-distribution - *may* be signalled by any of the operations when given an invalid distribution (e.g. doesn't sum to 1)
element-not-in-domain - signalled when trying to evaluate probability of an element that isn't in the domain of the distribution
cond-dist-not-defined - signalled when trying to condition on an element for which the conditional dist is not defined.
rv-inverse-not-defined - signalled by inverse functions of random variables for undefined values

Constants
---------
*prob-dist-sum-tol* - valid distributions must sum to within this of 1.0

")
  
  (:nicknames prob)
  (:export <prob-dist>
	   [prob-dist]
	   [cond-prob-dist]
	   <cond-prob-dist>
	   [random-variable]
	   <rv-dist>
	   <sampling-without-replacement>
	   <sampling-with-replacement>
	   make-multinomial-dist
	   make-boltzmann-dist
	   make-deterministic-dist
	   make-unif-dist-over-set
	   sample-uniformly
	   sample-multinomial
	   make-rv-dist
	   make-invertible-rv-dist
	   make-mixture-dist
	   standardize-alist-prob-dist
	   sample
	   prob
	   is-valid-prob-dist
	   map-prob-dist
	   updatef
	   clone-dist
	   cond-prob
	   cond-dist
	   cond-sample
	   cond-exp
	   evaluate-rv
	   expectation
	   sample-iterator
	   sample-space
	   do-sample-points
	   invalid-distribution
	   cond-dist-not-defined
	   rv-inverse-not-defined
	   element-not-in-domain)
  (:use common-lisp
	utils
	set))


(in-package probability)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <prob-dist> () ())
(defclass <cond-prob-dist> () ())  


(defun is-assoc-list (x)
  (and (listp x)
       (every #'consp x)))

(deftype [prob-dist] ()
  "Type for probability distributions.  Can be either

1. A vector of numbers, representing a multinomial distribution.  E.g., #(.2 .5 .3) assigns probability .2 to 0, .5 to 1, and .3 to 2
2. An association list with items of the form (ELT . PROB).  The ELT must all be different (use standardize-alist-prob-dist otherwise)
3. Object of type <prob-dist>"
  `(or <prob-dist> vector (satisfies is-assoc-list) ))


(deftype [cond-prob-dist] ()
  "Type for conditional probability distributions of the form P(Y|X).  Can be either

1.  A function of one argument,  (the values of the variable being conditioned on), which returns a [prob-dist].
2.  Object of type <cond-prob-dist>

If you want to condition on multiple variables, you need to combine them into a single object, such as a list.  Make sure that the combined object has the right behaviour under equalp.

Conditional distributions should never modify the object being conditioned on."
  `(or <cond-prob-dist> function))




(deftype [random-variable] ()
  "Type for random variables, in the measure-theoretic sense of a function defined on a sample space.  The point of having this type is to allow structured representations which allow the basic operations to be done more efficiently.  Right now, can only be a real-valued function."
  'function)


(define-condition invalid-distribution () 
  ((p :initarg :p :reader p)))

(define-condition element-not-in-domain ()
  ((x :initarg :x :reader x)))

(defun signal-element-not-in-domain (x)
  "signal-element-not-in-domain X.  Signal, then return 0."
  (signal 'element-not-in-domain :x x)
  0.0)
	  

(define-condition cond-dist-not-defined ()
  ((dist :initarg :dist :reader dist)
   (x :initarg :x :reader x)))

(define-condition rv-inverse-not-defined ()
  ((y :initarg :y :reader y)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant *prob-dist-sum-tol* .0001)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; operations for probability distributions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defgeneric sample (p)
  (:documentation "sample PROB-DIST.  Return a randomly chosen element sampled according to PROB-DIST."))

(defgeneric prob (p x)
  (:documentation "prob PROB-DIST ELT.  Evaluate the probability of ELT.  If the element is not in the set, might throw an element-not-in-domain condition, and then return 0 (i.e. you only need to worry about handling the condition if you want to override this behaviour)"))

(defgeneric expectation (p rv)
  (:documentation "expectation PROB-DIST RANDOM-VAR.  Compute the expectation of RANDOM-VAR."))

(defgeneric sample-space (p)
  (:documentation "Return a [set], representing the sample space of this distribution."))

(defgeneric sample-iterator (p)
  (:documentation "sample-iterator PROB-DIST.  Returns a function F.  Calling F returns 1) the next sample point, if it exists, or an arbitrary value otherwise 2) The probability of the item, if it exists, or nil otherwise 3) T if there were no more elements.  The first and second return values only make sense if the third one is NIL.  A standard way to use sample-iterator is via do-sample-points.  Default method just uses iterator over sample-space.")
  (:method ((p t))
	   (let ((iter (iterator (sample-space p))))
	     (lambda ()
	       (multiple-value-bind (item done?)
		   (funcall iter)
		 (if done?
		     (values nil nil t)
		   (values item (prob p item) nil)))))))

(defmacro do-sample-points ((item-var prob-var p &optional result-form) &body body)
  "macro do-sample-points (ITEM-VAR PROB-VAR PROB-DIST &optional RESULT-FORM) &body BODY.  Loop over the elements of the sample space of PROB-DIST.  During BODY, ITEM-VAR is bound to successive sample points, and PROB-VAR to the corresponding probability.  If RESULT-FORM is provided, this is the return value.  Otherwise, return NIL.

Note : this is not very efficient for hashtables - use map-prob-dist instead."
  (with-gensyms (iter done)
    `(loop
	 with ,iter = (sample-iterator ,p)
	 with (,item-var ,prob-var ,done)
	      
	 do (multiple-value-setq (,item-var ,prob-var ,done)
	      (funcall ,iter))
	 when ,done do (return ,result-form)
		       
	 do ,@body)))

(defgeneric is-valid-prob-dist (p)
  (:documentation "is-valid-prob-dist P.  Return true iff P is a valid probability distribution, i.e. that it is positive and sums to 1 over its domain.  Need not be implemented by all subtypes."))


(defgeneric map-prob-dist (fn dist)
  (:documentation "map-prob-dist RESULT-TYPE FN DIST.  FN is a function of two arguments : an item and its probability.  DIST is the probability distribution.  Iterates over sample space of distribution applying FN to each element and its probability.  Returns nil.  Default method just calls do-sample-points.

Not as general as do-sample-points.  Mainly used for hash table distributions, for efficiency.")

  (:method (fn dist)
	   (do-sample-points (x p dist nil)
	     (funcall fn x p))))
  
	      

(defgeneric update-dist (p1 p2 eta)
  (:documentation "update-dist DIST1 DIST2 ETA.  Not implemented for all distributions.  Destructively update DIST1 to (1-ETA)*DIST1 + ETA*DIST2 and return it.  Used by updatef, which is the function that should be called by outside code."))

(defmacro updatef (p1 p2 eta)
  "macro updatef P1 P2 ETA.  Update distribution P1 to ETA*P2 + (1-ETA)*P1.  Not implemented by all distributions."
  `(_f update-dist ,p1 ,p2 ,eta))

(defgeneric clone-dist (p)
  (:documentation "clone-dist P.  The generic function clone can be applied to probability distributions, but may not always behave as desired.  For example, given an association list, clone will clone both the keys and values, whereas, if the alist is being thought of as a probability distribution, it is not necessary to clone either.  The behaviour of clone-dist is
1) For alists, just create a new alist structure, but don't clone the keys or values
"))
  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; operations for conditional probability distributions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defgeneric cond-dist (p x)
  (:documentation "cond-dist P X.  Returns a [prob-dist] representing the conditional distribution given X.  May throw a cond-dist-not-defined condition.

Should never modify the object being conditioned on, for example in generating the possible values of the new variable.")
  (:method ((p function) x)
	   (handler-case
	       (funcall p x)
	     (cond-dist-not-defined ()
	       (error 'cond-dist-not-defined :x x :cond-dist p)))))


(defgeneric cond-prob (p y x)
  (:documentation "cond-prob P Y X.  Returns the conditional probability of Y given X.  May throw an element-not-in-domain or cond-dist-not-defined condition if either Y or X is illegal.  By default, just call cond-dist then evaluate the result on Y.

Implementors should never modify X.")
  (:method ((p t) y x)
	   (prob (cond-dist p x) y)))

(defun cond-sample (p x)
  "cond-sample P X.  Sample from P(Y|X).  Might throw a cond-dist-not-defined condition.

Implementors should never modify the object X."
  (sample (cond-dist p x)))


(defgeneric cond-exp (p rv &optional force-eval)
  (:documentation "cond-exp COND-DIST RV &optional (FORCE-EVAL nil).  Suppose COND-DIST is a conditional distribution P(Y|X), and RV is a random variable on Y.  Returns a random variable f on X, where f(x) = E(RV(Y)|X=x).  This operation occurs frequently in Bellman equations for Markov decision processes.  Default operation just returns a function that does the expectation whenever it's called on some specific X.  Subclasses might take advantage of structure in P and RV.  

Setting FORCE-EVAL true indicates a preference for doing as much computation as possible when this method is called, so that carrying out operations on the returned random variable is fast.  Methods may or may not take this into account.

Might throw a cond-dist-not-defined condition.")
  (:method (p rv &optional (force-eval nil))
	   (assert (not force-eval) () "default method for cond-exp does not support FORCE-EVAL right now.")
	   (lambda (x) (expectation (cond-dist p x) rv))))


(defun make-cond-dist-from-matrix (a)
  "make-cond-dist-from-matrix A.  Treat A as a 'transition matrix' and return the corresponding conditional distribution."
  (let ((num-cols (array-dimension a 1))
	(last-row (1- (array-dimension a 0))))
    (lambda (i)
      (check-type i fixnum)
      (assert (between i 0 last-row))
      (make-array num-cols :displaced-to a :displaced-index-offset (* i num-cols)))))

    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; random variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric evaluate-rv (rv x)
  (:documentation "evaluate-rv RV X.  Evaluate this random variable at sample point X."))