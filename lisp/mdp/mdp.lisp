(in-package mdp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Class definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <mdp> (<smdp>)
  ()
  (:documentation "abstract class for Markov Decision Processes

Initargs
:state-set

Subclasses include
<tabular-mdp>.

Subclasses must
- set a value for state-set
- implement action-set or avail-actions
- define terminal? if necessary
- implement trans-dist, trans-prob if necessary, and reward"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defgeneric trans-dist (m s a)
  (:documentation "trans-dist MDP STATE ACTION.  Transition distribution as a result of doing ACTION at STATE."))

(defgeneric trans-prob (m s a d)
  (:documentation "trans-prob MDP SOURCE ACTION DEST.  Probability of transitioning from SOURCE to DEST given ACTION.  Default method just uses the trans-dist.")
  (:method ((m <mdp>) s a d)
	   (prob (trans-dist m s a) d)))

(defgeneric reward (m s a d)
  (:documentation "reward MDP SOURCE ACTION DEST.  Reward for transitioning from SOURCE to DEST given ACTION."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; An MDP is a special case of an SMDP.  So MDPs need to implement
;; smdp-trans-dist.  Note that this wrapper code is quite inefficient.
;; If an MDP is going to be used as an SMDP, it is better to just implement it
;; as such.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <mdp-trans-dist> (<prob-dist>)
  ((mdp :reader mdp
	:initarg :mdp)
   (next-state-dist :reader dist
		    :initarg :dist)
   (source :reader s
	   :initarg :s)
   (action :reader a
	   :initarg :a))
  (:documentation "Wrapper that turns a row of the mdp transition distribution (over next state) into an SMDP transition distribution (over next-state, reward, and duration)."))

(defmethod sample ((d <mdp-trans-dist>))
  (let ((s2 (sample (dist d))))
    (make-outcome s2 (reward (mdp d) (s d) (a d) s2) 1)))

(defmethod prob ((d <mdp-trans-dist>) outcome)
  (let ((s2 (outcome-state outcome)))
    (if (and (eql (outcome-reward outcome) (reward (mdp d) (s d) (a d) s2))
	     (eql (outcome-duration outcome) 1))
	(prob (dist d) s2)
      0)))

(defmethod expectation ((d <mdp-trans-dist>) f)
  (let ((m (mdp d))
	(s (s d))
	(a (a d)))
    (expectation 
     (dist d)
     (lambda (s2)
       (evaluate-rv f (make-outcome s2 (reward m s a s2) 1))))))

(defmethod smdp-trans-dist ((m <mdp>) s a)
  (make-instance '<mdp-trans-dist> :dist (trans-dist m s a) :mdp m :s s :a a))


(defmethod sample-iterator ((d <mdp-trans-dist>))
  (let ((iter (set:iterator (state-set (mdp d)))))
    (lambda ()
      (multiple-value-bind (s2 done?)
	  (funcall iter)
	(if done?
	    (values nil nil t)
	  (values (make-outcome s2
				(reward (mdp d) (s d) (a d) s2)
				1)
		  (prob (dist d) s2) 
		  nil))))))
      
  



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other operations - these are implemented here (but
;; can also be overridden)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defgeneric transition-matrix (m)
  (:documentation "transition-matrix MDP.  Requires state and action sets to be [numbered-set]s.  In this case, return the transition matrix, indexed using the numbering on the states and actions.  Default method just enumerates over states and actions to generate the matrix.")
  (:method ((m <mdp>))
	   (let* ((states (state-set m))
		  (actions (action-set m))
		  (num-states (size states))
		  (num-actions (size actions))
		  (trans-matrix (make-array (list num-states num-actions num-states))))
	     (assert (and (typep states '[numbered-set]) (typep actions '[numbered-set]))
		 nil "State and action sets of MDP must be of type [numbered-set] in order to create transition matrix")
	     (do-elements (s states trans-matrix i)
	       (do-elements (a actions nil j)
		 (do-elements (s2 states nil k)
		   (setf (aref trans-matrix i j k)
		     (trans-prob m s a s2))))))))

(defgeneric reward-matrix (m)
  (:documentation "reward-matrix MDP.  Requires state and action sets to be [numbered-set]s.  In this case, return the reward matrix, indexed using the numbering on the states and actions.  Default method just enumerates over states and actions to generate the matrix.")
  (:method ((m <mdp>))
	   (let* ((states (state-set m))
		  (actions (action-set m))
		  (num-states (size states))
		  (num-actions (size actions))
		  (rew-matrix (make-array (list num-states num-actions num-states))))
	     (assert (and (typep states '[numbered-set]) (typep actions '[numbered-set]))
		 nil "State and action sets of MDP must be of type [numbered-set] in order to create reward matrix")
	     (do-elements (s states rew-matrix i)
	       (do-elements (a actions nil j)
		 (do-elements (s2 states nil k)
		   (setf (aref rew-matrix i j k)
		     (reward m s a s2))))))))




