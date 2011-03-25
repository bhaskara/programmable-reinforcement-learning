(in-package mdp-env)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; class def
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <2tbn-mdp> (mdp:<mdp>)
  ((2tbn :initarg :2tbn :reader 2tbn :type bnet:<2tbn>))
  (:documentation "Represents an MDP described by a 2TBN.  Usually, we're more interested in a corresponding <mdp-env> object, which can be created using make-2tbn-mdp-env.  The mdp itself can then be retrieved using the mdp method of the env object."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mdp ops
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod mdp:trans-dist ((m <2tbn-mdp>) s a)
  (prob:cond-dist (2tbn m) (cons s a)))

(defmethod mdp:reward ((m <2tbn-mdp>) s a d)
  (bnet:reward (2tbn m) s a d))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constructor for mdp-envs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-2tbn-mdp-env (2tbn &key (term-pred (constantly nil)))
  "make-2tbn-mdp-env 2TBN &key TERM-PRED.  Create an object of type <mdp-env>.

2TBN - the 2tbn that specifies the transition and reward models
TERM-PRED - function that takes a state and returns T iff it's terminal (it's also possible to subclass and add a method for terminal? instead of using this)

If the function current-effectors is ever going to be called on the returned environment, then 2TBN should satisfy certain conditions:
- a method for effectors should be defined for possible states (so the state representation in the 2tbn should probably be a structure)
- actions in the 2tbn should be represented by a list (in increasing order of effector id)."

  (make-instance '<mdp-env> 
    :mdp (make-instance '<2tbn-mdp> :2tbn 2tbn :term-pred term-pred
			:state-set (bnet:state-set 2tbn)
			:action-set (bnet:action-set 2tbn))
    :init-dist (bnet:init-dist 2tbn)))
		 
