(defpackage exploration-policy
  (:documentation "Package exploration-policy (exp-pol)

Types
- <exp-pol>
- <boltzmann-exp-pol>

Operations that can be called on an exploration policy
- make-choice
- choice-dist
- reset-counts
- reset-table

Subclasses of <exp-pol> must implement
- get-choice-dist

Other
- make-temp-decay-fn
- make-linear-epsilon-decay-fn
")
  (:export
   <exp-pol>
   <boltzmann-exp-pol>
   <epsilon-boltzmann-exp-pol>
   
   make-choice
   choice-dist
   reset-counts
   reset-table
   
   make-temp-decay-fn
   make-linear-epsilon-decay-fn
   
   get-choice-dist)
  (:use
   cl
   policy
   utils
   prob)
  (:nicknames exp-pol))

(in-package exp-pol)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; class def
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <exp-pol> (<stochastic-policy>)
  ((bucket-function :initarg :bucket-fn :initform #'canonicalize :reader bucket-fn)
   (test :initarg :initarg :initform #'equal :reader test)
   (count-table :writer set-count-table :reader count-table))
  (:documentation "Class for exploration policies.  Subclass of <stochastic-policy>.

Create using make-instance with initialization arguments
:bucket-fn - function that maps a state to a bucket.  States whose buckets are equal under the test will share the count used in the exploration policy.  By default, is #'canonicalize
:test - function used to compare bucket ids.  Default is #'equal."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((pol <exp-pol>) &rest args)
  (reset-table pol))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; exported operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun reset-table (pol)
  "reset-table EXP-POL.  Make the count table of the policy be a new empty hash table.  See also reset-counts."
  (set-count-table (make-hash-table :test (test pol)) pol))

(defun reset-counts (pol)
  "reset-counts EXP-POL.  Reset the count of each bucket currently in the policy's table to 0.  Preferable to reset-table if many of the same buckets are likely to be seen again."
  (let ((h (count-table pol)))
    (maphash #'(lambda (k v) (declare (ignore v)) (setf (gethash k h) 0)) h)))

(defmethod choice-dist ((pol <exp-pol>) omega)
  (get-choice-dist 
   pol omega
   (get-and-inc-count 
    pol (funcall (bucket-fn pol) omega))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; subclasses must implement
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric get-choice-dist (pol omega count)
  (:documentation "get-choice-dist EXP-POL OMEGA COUNT.  Return a distribution over choices at OMEGA.  COUNT is a nonnegative integer representing how many times the bucket corresponding to this state has previously been seen since the policy was last reset."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; internal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-and-inc-count (pol bucket)
  (let* ((h (count-table pol))
	 (count (or (gethash bucket h) 0)))
    (setf (gethash bucket h) (1+ count))
    count))
