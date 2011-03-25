;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data-struct/bnet/2tbn.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package bnet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variable descriptions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (2tbn-var-desc (:conc-name desc-))
  "A var-desc is structure for describing a single (state, action, or reward) variable of a 2TBN.  To create, use make-var-desc, with keywords
:id - the unique id of the variable (comparisons using #'equalp)
:domain - [set] over which this variable ranges. 

In addition, for state and reward variables, include the keywords
:prev-slice-parents - list of parents (by name) in the previous time slice (parents of variables are always states or actions)
:curr-slice-parents - list of parents in the current time slice

In addition, for reward variables, include the keyword
:reward-fn - function called on parents values (in a list, in order) to yield this variable's value.

In addition, for state variables, include the keywords
:trans-dist - [cond-prob-dist] of variable given parent values.  Assume parent values are in a list, in order.
:init-parents - list of parents for the initial time slice distribution
:init-dist - [cond-prob-dist] of variable given parents values.

The usual requirement about conditional distributions applies - they should never modify any of the objects they condition on."



  id
  domain
  (prev-slice-parents ())
  (curr-slice-parents ())
  (reward-fn 'undefined)
  (trans-dist 'undefined)
  (init-slice-parents ())
  (init-dist 'undefined))

;; how to clone var-descs
(def-struct-clone-method 2tbn-var-desc make-2tbn-var-desc desc-
			 (prev-slice-parents curr-slice-parents init-slice-parents)
			 (id domain trans-dist reward-fn init-dist))


(defun make-param-desc (name val)
  "make-param-desc NAME VAL
Create a description of a state variable that is actually a parameter - that is, it doesn't change over time.  It's just being included in the state description for convenience.  This is achieved by making a variable with only a single possible value, namely VAL, and whose init distribution and trans distributions have no parents and deterministically return VAL."
  (flet ((dist (par) (declare (ignore par)) `((,val . 1))))
    (make-2tbn-var-desc :id name :domain (list val)
			:init-slice-parents nil :init-dist #'dist
			:prev-slice-parents nil :curr-slice-parents nil
			:trans-dist #'dist)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro make-cpd (vars &body body)
  "make-cpd VARS &rest BODY.

VARS (not evaluated) - a list of variable names
BODY - forms surrounded by an implicit progn

Make a cpd function that takes in a single argument (a list), and destructures it into the vars, then executes body."
  
  (let ((par (gensym)))
    `(lambda (,par)
       (destructuring-bind ,vars ,par
	 ,@body))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2tbn class desc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  

(defclass <2tbn> (<cond-prob-dist>)
  ((num-state-vars :writer set-num-state-vars :reader num-state-vars :type fixnum)
   (num-reward-vars :writer set-num-reward-vars :reader num-reward-vars :type fixnum)
   (num-action-vars :writer set-num-action-vars :reader num-action-vars)
   (var-descs :writer set-descs :reader var-descs :type simple-vector)
   (state-acc :reader state-acc :writer set-state-acc)
   (reward-acc :reader reward-acc :writer set-reward-acc)
   (action-acc :reader action-acc :writer set-action-acc)
   (topological-order :writer set-top-order :reader top-order :type simple-vector)
   (init-topological-order :writer set-init-top-order :reader init-top-order :type simple-vector)
   (init-dist :writer set-init-dist :reader init-dist :type [prob-dist])
   (state-set :writer set-state-set :reader state-set :type [set])
   (action-set :writer set-action-set :reader action-set :type [set]))

  (:documentation "This class represents 2TBNs with actions and rewards.  It can therefore be used to represent influence diagrams, DBNs, Factored MDPs, etc.  

A 2TBN consists of three types of variables - state, action, and reward.  See package documentation for allowed operations.
To create a 2tbn, call (make-instance '<2tbn> ...) with the following initargs
:state-descs - list of descriptions of state vars (of type 2tbn-var-desc)
:reward-descs - list of descriptions of reward vars.  Optional, nil by default.
:action-descs - list of descriptions of action vars.  Optional, nil by default.
:state-acc - instantiation-accessor for state variables (see the inst-vars package documentation).  If not specified, defaults to storing state variables in a vector.
:reward-acc - instantiation accessor for reward variables.  If not specified, defaults to using a vector.
:action-acc - instantiation accessor for action variables.  If not specified, defaults to using a vector."))

(setf (documentation #'state-set 'function) "state-set 2TBN.  Set of instantiations to state variables."
      (documentation #'action-set 'function) "action-set 2TBN.  Set of instantiations to action variables.")
  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dist over next timeslice state given some specific s and a
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <2tbn-cond-dist> (<prob-dist>)
  ((2tbn :initarg :2tbn :reader 2tbn)
   (cached-sample-space :accessor cached-sample-space 
			:initform 'not-cached :type [set])
   (prev-s :initarg :prev-s :reader prev-s)
   (prev-a :initarg :prev-a :reader prev-a)))



(defmethod cond-dist ((2tbn <2tbn>) x)
  (let ((act (cdr x))
	(act-set (action-set 2tbn)))
    (assert (member? act act-set) ()
      "~a is not in the action set ~a of 2tbn" act act-set)
    (make-instance '<2tbn-cond-dist> :2tbn 2tbn 
		   :prev-s (car x) :prev-a act)))


(defmethod prob ((p <2tbn-cond-dist>) s)
  (loop
      with 2tbn = (2tbn p)
      with prev-s = (prev-s p)
      with prev-a = (prev-a p)
      with prod = 1
      with acc = (state-acc 2tbn)
		   
      for i below (num-state-vars 2tbn)
      for desc across (var-descs 2tbn)
			
      for val = (get-var-val acc s i)
      for par = (collect-parents desc prev-s prev-a s 2tbn)
      for dist = (desc-trans-dist desc)

      do (multf prod (cond-prob dist val par))
	     
      finally (return prod)))




(defmethod sample ((p <2tbn-cond-dist>))
  (loop
      with 2tbn = (2tbn p)
      with s = (create-inst (state-acc 2tbn))
      with prev-s = (prev-s p)
      with prev-a = (prev-a p)
      with descs = (var-descs 2tbn)
	       
      for i in (top-order 2tbn)
      repeat (num-state-vars 2tbn)
      for desc = (aref descs i)
  		  
      do (let ((par (collect-parents desc prev-s prev-a s 2tbn))
	       (dist (desc-trans-dist desc)))
	   (set-2tbn-var-val 2tbn s nil i (cond-sample dist par)))
	 
      finally (return s)))

(defmethod sample-space ((p <2tbn-cond-dist>))
  (let ((cached-version (cached-sample-space p)))
    (if (eq cached-version 'not-cached)
	(setf (cached-sample-space p)
	  (let* ((2tbn (2tbn p))
		 (prev-s (prev-s p))
		 (prev-a (prev-a p))
		 (descs (var-descs 2tbn)))
	    (flet ((all-successors (x i)
		     (let* ((desc (aref descs i))
			    (par (collect-parents desc prev-s prev-a x 2tbn))
			    (dist (desc-trans-dist desc))
			    (ss nil))
		       (do-elements (v (sample-space (cond-dist dist par)) ss)
			 (let ((y (clone x)))
			   (set-2tbn-var-val 2tbn y nil i v)
			   (push y ss))))))
	      (loop
		  with ss = (list (create-inst (state-acc 2tbn)))
		   
		  for i in (top-order 2tbn)
		  repeat (num-state-vars 2tbn)
		 
		  do (setf ss 
		       (apply #'append
			      (mapcar (lambda (x) (all-successors x i)) ss)))
	   
		  finally (return ss)))))
      
      cached-version)))
	       
	
  
			 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; reward function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun reward (2tbn prev-s prev-a s)
  "reward 2TBN S A S2.  Compute the reward for going from S to S2 after doing A.  The first returned value is the total reward, and the second is all the reward variables (in whatever format the tbn uses)."
  (loop

      with rews = (create-inst (reward-acc 2tbn))
      with descs = (var-descs 2tbn)
      with tot-rew = 0

      for j from (num-state-vars 2tbn)
      repeat (num-reward-vars 2tbn)
      for desc = (aref descs j)
  		  
      do (let ((par (collect-parents desc prev-s prev-a s 2tbn))
	       (f (desc-reward-fn desc)))
	   (let ((r (funcall f par)))
	     (set-2tbn-var-val 2tbn nil rews j r)
	     (incf tot-rew r)))
	 
      finally (return (values tot-rew rews))))
  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the initial distribution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <2tbn-init-dist> (<prob-dist>)
  ((2tbn :initarg :2tbn :reader 2tbn)))


(setf (documentation 'init-dist 'function)
  "init-dist 2TBN.  Return a [prob-dist] representing the distribution of the initial timestep of a 2tbn.")


(defmethod prob ((p <2tbn-init-dist>) x)
  (loop 
      with prod = 1
      with 2tbn = (2tbn p)
      with acc = (state-acc 2tbn)
		  
      for i below (num-state-vars 2tbn)
      for desc across (var-descs 2tbn)
		      
      for val = (get-var-val acc x i)
      for par = (collect-init-parents desc x 2tbn)
      for dist = (desc-init-dist desc)
		 
      do (multf prod (cond-prob dist val par))
	 
      finally (return prod)))


(defmethod sample ((p <2tbn-init-dist>))
  (loop
      with 2tbn = (2tbn p)
      with s = (create-inst (state-acc 2tbn))
      with descs = (var-descs 2tbn)
		   
      for i in (init-top-order 2tbn)
      for j below (num-state-vars 2tbn)
      ;;; use fact that top order is states first
      for desc = (aref descs i)
		 
		 
      do (let ((par (collect-init-parents desc s 2tbn))
	       (dist (desc-init-dist desc)))
	   (set-2tbn-var-val 2tbn s nil i (cond-sample dist par)))
	 
      finally (return s)))


(defmethod sample-space ((p <2tbn-init-dist>))
  (filter 'list (state-set (2tbn p))
	  (lambda (s) (> (prob p s) 0))))
		 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; internal fns for collecting parent values into a vector
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun collect-parents (desc prev-s prev-a s 2tbn)
  (nconc
   (collect-parents-helper 2tbn prev-s prev-a (desc-prev-slice-parents desc))
   (collect-parents-helper 2tbn s nil (desc-curr-slice-parents desc))))

(defun collect-init-parents (desc s 2tbn)
  (collect-parents-helper 2tbn s nil (desc-init-slice-parents desc)))

(defun collect-parents-helper (2tbn s a pars)
  (mapcar
   (lambda (p)
     (let ((v (get-state-or-action-val 2tbn s a p)))
       (assert v () "Nil value received when getting value ~a for state ~a and action ~a" p s a)
       v))
   pars))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; methods for accessing state, reward, and action 
;; variables 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-state-or-action-val (2tbn state action ind)
  (let ((num (num-state-vars 2tbn)))
    (if (< ind num)
	(get-var-val (state-acc 2tbn) state ind)
      (get-var-val (action-acc 2tbn) action (- ind num (num-reward-vars 2tbn))))))

(defun get-state-or-reward-val (2tbn state reward ind)
  (let ((num (num-state-vars 2tbn)))
    (if (< ind num)
	(get-var-val (state-acc 2tbn) state ind)
      (get-var-val (reward-acc 2tbn) reward (- ind num)))))

(defun set-2tbn-var-val (2tbn state reward ind val)
  (let ((num (num-state-vars 2tbn)))
    (if (< ind num)
	(set-var-val (state-acc 2tbn) state ind val)
      (set-var-val (reward-acc 2tbn) reward (- ind num) val))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constructor and helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((2tbn <2tbn>) &rest args 
				       &key state-descs (action-descs nil) (reward-descs nil)
					    (state-acc (make-vec-accessors (length state-descs)))
					    (reward-acc (make-vec-accessors (length reward-descs)))
					    (action-acc (make-vec-accessors (length action-descs))))

  ;; concatenate descriptions into single list
  (let ((descs (map 'vector (lambda (x) (if (listp x) (apply #'make-2tbn-var-desc x) (clone x)))
		    (concatenate 'vector state-descs reward-descs action-descs))))
    
    ;; set up index from variable id to numerical position in descs
    (labels 
	((lookup (id)
	   (let ((ids (map 'vector #'desc-id descs)))
	     (let ((pos (position id ids :test #'equal)))
	       (assert pos () "Variable named ~a not found in list ~a" id ids)
	       pos)))
	 (lookupf-seq (v)
	   (map-into v #'lookup v)))
	   
      ;; go over the descriptions and replace ids with numbers
      (map nil (lambda (desc)
		 (lookupf-seq (desc-curr-slice-parents desc))
		 (lookupf-seq (desc-prev-slice-parents desc))
		 (lookupf-seq (desc-init-slice-parents desc)))
	   descs)

      ;; set fields in 2tbn
      (set-descs descs 2tbn)
      (set-state-acc state-acc 2tbn)
      (set-reward-acc reward-acc 2tbn)
      (set-action-acc action-acc 2tbn)

      ;; keep track of number of each type of variable
      (set-num-state-vars (length state-descs) 2tbn)
      (set-num-reward-vars (length reward-descs) 2tbn)
      (set-num-action-vars (length action-descs) 2tbn)
		      

      ;; precompute the topological orders for the trans and init distributions
      (set-top-order 
       (topological-sort (map 'vector #'desc-curr-slice-parents descs))
       2tbn)
      (set-init-top-order
       (topological-sort (map 'vector #'desc-init-slice-parents descs))
       2tbn)
      
      ;; initial timeslice distribution
      (set-init-dist (make-instance '<2tbn-init-dist> :2tbn 2tbn) 2tbn)
      
      ;; state and action sets
      (set-state-set (make-instance '<prod-set>
		       :sets (mapcar #'desc-domain state-descs)
		       :inst-acc (state-acc 2tbn))
		     2tbn)
      (set-action-set 
       (if (eq (action-acc 2tbn) 'single-object)
	   (progn
	     (assert (= (length action-descs) 1))
	     (desc-domain (first action-descs)))
	 (make-instance '<prod-set>
	   :sets (mapcar #'desc-domain action-descs)
	   :inst-acc (action-acc 2tbn))) 
       2tbn))))
      




  