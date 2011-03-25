(defpackage alisp-gold-standard
  (:documentation "Defines the <alisp-gold-standard> reinforcement learning algorithm.

Exports
-------
<alisp-gold-standard>
make-alisp-gold-standard-learning-alg")
  (:export
   <alisp-gold-standard>
   make-alisp-gold-standard-learning-alg)
  (:use 
   cl
   set
   alisp-obs
   utils)
  (:import-from 
   mdp
   make-outcome
   outcome-state
   outcome-duration
   outcome-reward)
  )
		

(in-package alisp-gold-standard)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Class def
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass <alisp-gold-standard> (<q-learning-algorithm> <alisp-learning-algorithm>)
  ((discount :type float
	     :initarg :discount
	     :initform 1.0
	     :reader discount)
   (state-set :type [numbered-set]
	      :accessor state-set
	      :documentation "Numbered set of states.  Numbers are used as indices in counts.  The first state is always the special symbol 'dummy-terminal-state.")
   (counts :type vector
	   :accessor counts
	   :documentation "A vector mapping state numbers to entries.  Each entry is a cons of a numbered-set of choices and a vector that maps choices at the joint state (assumed to be integers) to count information.  Finally, the count information is a pair consisting of 1) how many times this choice has been observed 2) a hashtable mapping outcomes to integers.  An outcome is a triple of the form (OMEGA', R, DUR).  Note that state number 0 is a dummy terminal state, so the corresponding entry consists of a dummy choice that just returns to this state.")
   (previous-state :accessor prev-omega)
   (previous-choice :accessor prev-u)
   (previous-state-seen :accessor prev-omega-seen)
   (total-reward :accessor total-reward)
   (total-discount :accessor total-discount)
   (num-steps-since-choice :accessor num-steps-since-choice))
  
  (:documentation "Implements 'gold-standard' model-based reinforcement-learning for ALisp.  The algorithm maintains a maximum-likelihood estimate of the SMDP transition, and does DP using this estimate when asked for the current policy.

Assumes for now that choice sets are integers, where n represents the set {0,...,n-1}.
"))


(defconstant *mpi-k* 8)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; creation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod shared-initialize :after ((alg <alisp-gold-standard>) names &rest args)
  (declare (ignore names))
  (setf (state-set alg) (indexed-set:make-indexed-set #(dummy-terminal-state))
	(counts alg) (make-array 1 :adjustable t :fill-pointer 1)
	(prev-omega-seen alg) nil
	(aref (counts alg) 0) (make-dummy-terminal-entry)))

(defun make-alisp-gold-standard-learning-alg (&key (discount 1.0) (debug-str nil))
  "make-alisp-gold-standard-learning-alg &key (DISCOUNT 1.0) (DEBUG-STR nil)"
  (make-instance '<alisp-gold-standard> :discount discount :debug-str debug-str))


(defun make-dummy-terminal-entry ()
  (let ((a (make-array 1)))
    (setf (aref a 0)
      (cons 1
	    (let ((h (make-hash-table :test #'equalp)))
	      (setf (gethash (make-outcome 'dummy-terminal-state 0 1) h) 1)
	      h)))
    (cons 1 a)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operations from <alisp-observer>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod inform-env-step ((alg <alisp-gold-standard>) a r s2 term)
  
  (declare (ignore s2 a))
  (when (prev-omega-seen alg)
    (incf (total-reward alg) (* (total-discount alg) r))
    (multf (total-discount alg) (discount alg))
    (incf (num-steps-since-choice alg))

    
    ;; if environment has terminated, make a note of this
    (when term (observe-outcome alg (prev-omega alg) (prev-u alg)
				(total-reward alg) nil nil t))))

(defmethod inform-alisp-step ((alg <alisp-gold-standard>) omega u)
  (when (prev-omega-seen alg)
    (observe-outcome alg (prev-omega alg) (prev-u alg) (total-reward alg)
		       (num-steps-since-choice alg) omega nil))
    (notice-state alg omega)
    (setf (prev-omega-seen alg) t
	  (prev-omega alg) omega
	  (prev-u alg) u
	  (total-reward alg) 0.0
	  (total-discount alg) 1.0
	  (num-steps-since-choice alg) 0))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operations from <q-learning-algorithm>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod reset ((alg <alisp-gold-standard>))
  (reinitialize-instance alg))

(defmethod knowledge-state ((alg <alisp-gold-standard>) &optional (fresh t))
  "When asked for state of knowledge, the algorithm computes the current SMDP, then does dynamic programming and returns the SMDP and the Q-function in a list."
  
  (declare (ignore fresh)) ;; fresh is always treated as true
  (let ((m (create-smdp alg)))
    
    (multiple-value-bind (pol val)
	(dp:policy-iteration m :k *mpi-k* :discount (discount alg))
      (declare (ignore pol))
      (list m (dp:q-from-v m val (discount alg))))))



(defmethod get-q-fn ((alg <alisp-gold-standard>) ks)
  (second ks))

(defmethod get-smdp ((alg <alisp-gold-standard>) ks)
  (first ks))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smdp creation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun create-smdp (alg)
  (let* ((states (clone (state-set alg)))
	 (term-vec (make-array (size states) :element-type 'boolean :initial-element nil))
	 (avail-actions-vec (map 'vector #'car (counts alg)))
	 (trans-dists (map 'vector #'create-smdp-helper (counts alg))))
    
    (setf (aref term-vec 0) t)
    
    (flet ((avail-actions (s)
	     (aref avail-actions-vec (item-number s states))))
      (make-instance 'mdp:<smdp>
	:state-set states
	:avail-actions #'avail-actions
	:term-pred (lambda (s) (aref term-vec (item-number s states)))
	:trans-dist
	(lambda (x)
	  (let* ((s (car x))
		 (i (item-number s states))
		 (j (item-number (cdr x) (avail-actions s))))
	    (aref (aref trans-dists i) j)))))))


(defun create-smdp-helper (x)
  (map 'vector #'create-choice-dist (cdr x)))

(defun create-choice-dist (counts)
  (let ((total (car counts)))
    (if (eql total 0)
	;; if there are no observations, assume this choice results in terminating in time 1 with reward 0
	`((,(make-outcome 'dummy-terminal-state 0 1) . 1.0))
      
      ;; otherwise, normalize the counts of each observation
      (loop
	  for outcome being each hash-key in (cdr counts) using (hash-value num)
	  for out2 = (if (outcome-duration outcome) 
			 outcome 
		       (make-outcome 
			(outcome-state outcome)
			(outcome-reward outcome)
			1))
	  collect (cons out2 (/ num total))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; code relating to responding to observations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun notice-state (alg omega)
  "postcondition is that the state-set and counts for ALG include OMEGA"
  (let* ((counts (counts alg))
	 (choices (js-choices omega))
	 (num-choices (size choices))
	 (states (state-set alg)))
    (unless (member? omega states)
      (addf states omega t)
      (vector-push-extend
       (cons choices
	     (let ((a (make-array num-choices)))
	       (dotimes (i num-choices)
		 (setf (aref a i)
		   (cons 0 (make-hash-table :test #'equalp))))
	       a))

       counts))))


(defun observe-outcome (alg omega u r dur omega2 terminated?)
  "Assumes ALG has an entry for OMEGA.  If TERMINATED? is false, increments the count for observing (OMEGA', R, DUR) after doing U in OMEGA.  If TERMINATED? is true, increments the count for observing termination with reward R after doing U in OMEGA."
  
  (let* ((entry (if terminated? 
		    (make-outcome 'dummy-terminal-state r nil)
		  (make-outcome omega2 r dur)))
	 (state-counts (aref (counts alg) (item-number omega (state-set alg))))
	 (counts (aref (cdr state-counts) (item-number u (car state-counts))))
	 (count-table (cdr counts))
	 (num (gethash entry count-table)))
    (incf (car counts))
    (setf (gethash entry count-table)
      (aif num (incf it) 1)))
  
  (setf (prev-omega-seen alg) nil))




(in-package cl-user)

   

