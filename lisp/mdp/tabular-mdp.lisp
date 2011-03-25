;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mdp/tabular-mdp.lisp
;; representing mdps with arrays
;; TODO : probably obsolete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package mdp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Class definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <tabular-mdp> (<mdp>) 
  ((transition-matrix
    :type (simple-array float 3)
    :initarg :transition-matrix
    :reader transition-matrix)
   (reward-matrix
    :type (simple-array float 3)
    :initarg :reward-matrix
    :reader reward-matrix)
   (termination-vector
    :type (simple-array boolean 1)
    :initarg :termination-vector
    :reader termination-vector)
   (indexing-required
    :type boolean
    :initarg :index
    :reader index))

  (:documentation "A tabular mdp is one in which the transition, reward, and termination functions are represented using arrays.   Indexing is done using the numbering of the state and action sets.

Create using make-tabular-mdp."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constructor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun make-tabular-mdp (transition-matrix reward-matrix &key termination-vector state-set action-set fresh)
  "make-tabular-mdp TRANSITION-MATRIX REWARD-MATRIX &key TERMINATION-VECTOR STATE-SET ACTION-SET FRESH

Let N be the number of states and M the number of actions

TRANSITION-MATRIX : Array where the (i,j,k)th element is the probability of going from state #i to #k if we do  action #j
REWARD-MATRIX : Either a NxMxN array where the (i,j,k)th element is the reward of going from state #i to #k via doing action #j, or a NxM array where the (i,j)th element is the reward of doing #j in #i, or a N-vector, where the ith element is the reward of doing anything in #i.
TERMINATION-VECTOR : If provided, N-vector where the ith element is t iff state #i is terminal.  By default all nil.
STATE-SET.  If provided, the [numbered-set] of states.  If not provided, just use {0,1,...,N-1}.
ACTION-SET.  If provided, the [numbered-set] of actions.  If not provided, just use {0,1,...,M-1}.
FRESH.  nil by default.  If true, then the created MDP will make copies of the parameters (so that if the originals are modified, it won't change the mdp)."

  (let* ((num-states (array-dimension transition-matrix  0))
	 (num-actions (array-dimension transition-matrix 1))
	 (states (aif state-set (if fresh (clone it) it) num-states))
	 (actions (aif action-set (if fresh (clone it) it) num-actions))
	 (term (aif termination-vector 
		    (if fresh (clone it) it)
		    (make-array num-states :element-type 'boolean :initial-element nil)))
	 (rews (make-rew-matrix reward-matrix num-states num-actions fresh))
	 (ind (or state-set action-set)))

    (make-instance '<tabular-mdp>
      :state-set states :action-set actions :termination-vector term :index ind
      :transition-matrix transition-matrix :reward-matrix rews)))
      


(defun make-rew-matrix (rew ns na fresh)
  (let ((dims (array-dimensions rew)))
    (if (= (length dims) 3)
	(if fresh (clone rew) rew)
      (let ((rew-mat (make-array (list ns na ns) :element-type 'float)))
      
	(if (= (length dims) 2)
	    (dotimes (i ns)
	      (dotimes (j na)
		(dotimes (k ns)
		  (setf (aref rew-mat i j k)
		    (aref rew i j)))))
	  (dotimes (i ns)
	    (dotimes (j na)
	      (dotimes (k ns)
		(setf (aref rew-mat i j k)
		  (aref rew i))))))
	rew-mat))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; methods from mdp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod trans-prob ((m <tabular-mdp>) s a d)
  (if (index m)
      (aref (transition-matrix m) 
	    (get-state-num s m)
	    (get-action-num a m)
	    (get-state-num d m))
    (aref (transition-matrix m) s a d)))
	  
	    

(defmethod trans-dist ((m <tabular-mdp>) s a)
  (let* ((trans (transition-matrix m))
	 (states (state-set m))
	 (actions (action-set m)))
    (if (index m)
	(let ((i (get-state-num s m))
	      (j (get-action-num a m))
	      (dist nil))
	  (do-elements (s2 states dist k)
	    (push (cons s2 (aref trans i j k)) dist)))
      
      ;; otherwise, both states and actions are actually fixnums
      (make-array states
		  :displaced-to trans
		  :displaced-index-offset
		  (+ (* s states actions) (* a states))))))



(defmethod reward ((m <tabular-mdp>) s a d)
  (if (index m)
      (aref (reward-matrix m) (get-state-num s m) (get-action-num a m) (get-state-num d m))
    (aref (reward-matrix m) s a d)))

(defmethod terminal? ((m <tabular-mdp>) s)
  (if (index m)
      (aref (termination-vector m) (get-state-num s m))
    (aref (termination-vector m) s)))


