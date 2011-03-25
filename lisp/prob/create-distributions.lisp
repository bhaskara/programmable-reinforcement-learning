;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; prob/create-distributions.lisp
;; Ways of creating various standard distributions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package prob)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; deterministic distribution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-deterministic-dist (x)
  "make-deterministic-dist X.  Return a deterministic distribution over X (represented as an alist)."
  `((,x . 1.0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uniform distribution over a numbered set
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <numbered-set-dist> (<prob-dist>)
  ((numbered-set :type [numbered-set]
		 :reader s
		 :initarg :set))
  (:documentation "Represents a uniform distribution over a numbered set (maybe later allow arbitrary pf's on the set)"))

(defun make-unif-dist-over-set (s)
  "make-unif-dist-over-set SET.  Return a uniform distribution over set, which must be of type [numbered-set]."
  (check-type s [numbered-set])
  (assert (not (is-empty s)) () "Cannot make uniform distribution over empty set")
  (make-instance '<numbered-set-dist> :set s))

(defun sample-uniformly (s)
  "sample-uniformly SET.  Sample uniformly from the finite, numbered set SET."
  (item (random (size s)) s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod sample-space ((p <numbered-set-dist>))
  (s p))

(defmethod sample ((p <numbered-set-dist>))
  (sample-uniformly (s p)))

(defmethod prob ((p <numbered-set-dist>) x)
  ;; TODO signal condition when x is not in the set
  (declare (ignore x))
  (/ 1 (size (s p))))

(defmethod expectation ((p <numbered-set-dist>) rv)
  (let ((tot 0))
    (/ 
     (do-elements (x (s p) tot)
       (incf tot (evaluate-rv rv x)))
     (size (s p)))))
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dist gotten by composing a random variable with a prob dist
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <rv-dist> (<prob-dist>)
  ((dist :type [prob-dist] :reader dist :initarg :dist)
   (rv :type [random-variable] :reader rv :initarg :rv))
  (:documentation "Distribution gotten by composing a random variable with a prob dist.  Can create using make-rv-dist

Sampling is efficient.  Computing probabilities by default is not so efficient since it just iterates over the sample space of dist and checks when rv equals the given element."))

(defun make-rv-dist (rv dist)
  "make-rv-dist RAND-VAR PROB-DIST.  Compose RAND-VAR with PROB-DIST to get a new PROB-DIST of type <rv-dist>."
  (make-instance '<rv-dist> :rv rv :dist dist))

(defmethod prob ((p <rv-dist>) x)
  (let ((total-prob 0))
    (do-sample-points (y prob (dist p) total-prob)
      (when (same (evaluate-rv (rv p) y) x)
	(incf total-prob prob)))))


(defmethod sample ((p <rv-dist>))
  (evaluate-rv (rv p) (sample (dist p))))

(defmethod expectation ((p <rv-dist>) rv2)
  (let ((rv (rv p)))
    (expectation (dist p) 
		 (lambda (x) 
		   (evaluate-rv rv2 (evaluate-rv rv x))))))

(defmethod sample-iterator ((p <rv-dist>))
  (let ((iter (sample-iterator (dist p)))
	(already-seen nil))
    (labels
	((new-it ()
	   (multiple-value-bind (item prob done?)
	       (funcall iter)
	     (declare (ignore prob))
	     (if done?
		 (values nil nil t)
	       (let ((new-item (evaluate-rv (rv p) item)))
		 (if (member new-item already-seen :test #'same)
		     (funcall #'new-it)
		   (progn
		     (push new-item already-seen)
		     (values new-item (prob p new-item) nil))))))))
      #'new-it)))

(defmethod sample-space ((p <rv-dist>))
  
  (let ((iter (sample-iterator p))
	(l nil)
	(done? nil))
    (until done?
      (multiple-value-bind (item prob d2)
	  (funcall iter)
	(declare (ignore prob))
	(setf done? d2)
	(unless done? (push item l))))
    l))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; more efficient version of rv dists when the function is invertible
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass <invertible-rv-dist> (<rv-dist>)
  ((inverse :type function
	    :reader inverse
	    :initarg :inverse))
  (:documentation "A more efficient subclass of rv-dist for rv's with known inverse.  Create using make-inverse-rv-dist."))

(defun make-invertible-rv-dist (rv inverse dist)
  "make-invertible-rv-dist RV INVERSE DIST
INVERSE is a function of one argument that either returns the inverse value, or signals a 'rv-inverse-not-defined error."
  (make-instance '<invertible-rv-dist> :rv rv :inverse inverse :dist dist))

(defmethod prob ((p <invertible-rv-dist>) x)
  
  (handler-case
      (let ((y (funcall (inverse p) x)))
	(let ((z (evaluate-rv (rv p) y)))
	  
	  ;; to be really sure things are working right, we verify that the 
	  ;; image of the inverse under rv is the same as the original
	  (assert (same z x) ()
	    "~a not equal to image ~a of its inverse ~a."
	    x z y))
	
	(prob (dist p) y))
    
    
    ;; if inverse not defined, for now error (should really just signal a condition)
    (rv-inverse-not-defined ()
      (error 'element-not-in-domain :x x))))

(defmethod sample-iterator ((p <invertible-rv-dist>))
  ;; use invertibility of rv
  (let ((iter (sample-iterator (dist p))))
    (lambda ()
      (multiple-value-bind (x pr done)
	  (funcall iter)
	(if done
	    (values nil nil t)
	  (values (evaluate-rv (rv p) x) pr nil))))))


(defmethod sample-space ((p <invertible-rv-dist>))
  (let ((rv (rv p)))
    (if (functionp rv)
	(make-image-set (sample-space (dist p)) rv (inverse p))
      (call-next-method))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mixture model
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; class def
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <mixture-prob-dist> (<prob-dist>)
  ((weights :type simple-vector
	    :reader weights
	    :initarg :weights)
   (dists :type simple-vector
	  :reader dists
	  :initarg :dists))
  (:documentation "Subclass of <prob-dist> for mixture distributions.  Create using make-mixture-dist."))

(defun make-mixture-dist (weights dists)
  "make-mixture-dist WEIGHT-VECTOR DIST-VECTOR.  Create a mixture distribution over the distributions in DIST-VECTOR using weights in WEIGHT-VECTOR.  Doesn't 'expand out' the new distribution, which means the operations on the returned distribution take a bit longer."
  (let ((n (length weights)))
    (assert (= n (length dists)) () "Weights ~a and dists ~a must have same length" weights dists)
    (assert (is-valid-prob-dist weights)))
  (make-instance '<mixture-prob-dist> :weights weights :dists dists))
    


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 


(defmethod prob ((p <mixture-prob-dist>) y)
  (loop
      for w across (weights p)
      for d across (dists p)
      sum (* w (prob d y))))

(defmethod sample ((p <mixture-prob-dist>))
  (sample (aref (dists p) (sample (weights p)))))

(defmethod expectation ((p <mixture-prob-dist>) rv)
  (loop
      for w across (weights p)
      for d across (dists p)
      sum (* w (expectation d rv))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sample with replacement
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <sampling-with-replacement> (<prob-dist>)
  ((dist :type [prob-dist] :reader dist :initarg :dist
	 :documentation "The underlying distribution")
   (n :type fixnum :reader n :initarg :n
      :documentation "Number of samples."))
  (:documentation "A probability distribution that consists of sampling without replacement from some distribution DIST N times.  Results are collected into a vector of length n.

Initargs
:dist
:n"))

(defmethod prob ((p <sampling-with-replacement>) x)
  (let ((n (n p))
	(dist (dist p)))
    (if (and (arrayp x) (= (length x) n))
	(let ((prob 1))
	  (map nil (lambda (y) (multf prob (prob dist y))) x)
	  prob)
      (signal-element-not-in-domain x))))

(defmethod sample ((p <sampling-with-replacement>))
  (with-slots (n dist) p
    (mapset 'vector (lambda (i) (declare (ignore i)) (sample dist)) n)))

;; expectation not implemented for now

(defmethod sample-space ((p <sampling-with-replacement>))
  (make-instance 'prod-set:<var-set> :sets (loop repeat (n p) collect (sample-space (dist p)))))
		 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sample without replacement
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <sampling-without-replacement> (<prob-dist>)
  ((dist :type [prob-dist] :reader dist :initarg :dist
	 :documentation "The underlying distribution")
   (n :type fixnum :reader n :initarg :n
      :documentation "Number of samples"))
  (:documentation "A probability distribution that samples with replacement from DIST N times.  Results are collected into a vector of length N.

Initargs
:dist
:n

Note that sampling is done just by repeatedly trying to sample each element until we get one that's different.  So if the DIST assigns most or all of its probability to some M < N elements, this could go into a long or infinite loop."))

(defmethod initialize-instance :after ((p <sampling-without-replacement>) &rest args)
  ;; sanity check
  (let* ((s (sample-space (dist p)))
	 (n (n p))
	 (m (size s)))
    (assert (<= n m) ()
      "Cannot sample without replacement ~a times from set ~a of size ~a"
      n s m)))

(defmethod prob ((p <sampling-without-replacement>) x)
  (let ((n (n p)))
    (if (and (arrayp x) (= (length x) n))
	(loop
	    with dist = (dist p)
	    with prob = 1
	    with tot-prob = 1
		      
	    for y across x
	    for pr = (prob dist y)
	       
	    do (multf prob (/ pr tot-prob))
	       (decf tot-prob pr)
	 
	    finally (return prob))
      (signal-element-not-in-domain x))))

(defmethod sample ((p <sampling-without-replacement>))
  (loop
      with n = (n p)
      with dist = (dist p)
      with a = (make-array n)
	       
      for i below n
      for z = (loop
		  for y = (sample dist)
		  while (find y a :end i :test #'equalp)
		  finally (return y))
	
      do (assert z () "Can't sample without replacement from dist ~a which has () as a sample point" dist)
	 (setf (aref a i) z)
	 
      finally (return a)))


(defmethod sample-space ((p <sampling-without-replacement>))
  ;; this is an overestimate of the set, but that shouldn't cause a problem
  (make-instance 'prod-set:<var-set> :sets (loop repeat (n p) collect (sample-space (dist p)))))


;; expectation not implemented
      



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multinomial over a set
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <multinomial-dist> (<prob-dist>)
  ((prob-vec :type (vector float) :reader prob-vec :initarg :prob-vec)
   (domain :type [numbered-set] :reader domain :initarg :domain)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod prob ((p <multinomial-dist>) x)
  (aref (prob-vec p) (item-number x (domain p))))

(defmethod sample ((p <multinomial-dist>))
  (item (sample (prob-vec p)) (domain p)))

(defmethod expectation ((p <multinomial-dist>) rv)
  (let ((dom (domain p)))
    (expectation (prob-vec p) #'(lambda (i) (funcall rv (item i dom))))))

(defun sample-multinomial (domain &rest probs)
  (let ((x (random 1.0))
	(cumsum 0))
    (loop
	for p in probs
	for i from 0
	do (incf cumsum p)
	when (> cumsum x)
	do (return (item i domain)))))
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; creation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-multinomial-dist (v &optional (s (length v)))
  "make-multinomial-dist V &optional NUMBERED-SET
Make a multinomial distribution with probabilities as given by the vector.  V is a vector of nonnegative reals, and may be unnormalized.  If SET is not provided, it defaults to 0,1,..., N-1 where N is the length of the vector."
  (let ((z (reduce #'+ v)))
    (make-instance '<multinomial-dist>
      :prob-vec 
      
      (map 'vector 
		  #'(lambda (x) 
		      (assert (>= x 0) nil "Probability vector must be nonnegative.")
		      (/ x z))
		  v)
      :domain s)))

(defun make-boltzmann-dist (v temp &optional (domain (length v)))
  "make-boltzmann-dist VEC TEMPERATURE &optional DOMAIN.  Create boltzmann distribution from the vector of negative energies.  If DOMAIN is not provided, it defaults to 0,1,...,N-1 where N is the length of VEC"
  (let ((l (length v)))
    (assert (> l 0) (v)
      "Attempted to create Boltzmann distribution from empty vector.")
    
    (make-multinomial-dist
     (cond
      ((eql temp 'infty) (make-array l :element-type 'fixnum :initial-element 1))
      ((> temp 0) (map 'vector #'(lambda (x) (exp (/ x temp))) v))
      (t ;; temperature equals 0
       (let ((max-val (reduce #'max v)))
	 (map 'vector #'(lambda (x) (indicator (eql x max-val))) v))))
     domain)))


