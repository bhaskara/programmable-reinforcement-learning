(defpackage mdp

  (:use common-lisp
	set
	prob
	utils)
  (:export <mdp>
	   <smdp>
	   <tabular-mdp> 
	   <tabular-smdp>
	   <hierarchical-smdp>
	   <hierarchical-tabular-smdp>
	   make-tabular-mdp
	   tabular-smdp
	   sparsify-smdp
	   trans-prob
	   smdp-trans-dist
	   trans-dist
	   reward
	   transition-matrix
	   reward-matrix
	   state-set
	   num-states
	   avail-actions
	   sparsify
	   action-set
	   term-pred
	   terminal?
	   trans-vec
	   term-vec
	   num-avail-actions-vec
	   level
	   level-vec
	   make-outcome
	   outcome-state
	   outcome-reward
	   outcome-duration
	   outcome-p)
    (:documentation "Code relating to Markov Decision Processes.

Types
-----
<smdp>
<mdp>
<tabular-mdp>
<tabular-smdp>
<hierarchical-smdp>
<hierarchical-tabular-smdp>

Constructors
------------
make-tabular-mdp
tabular-smdp
sparsify-smdp
make-smdp-over-recursive-closure

(s)MDP Operations
-----------------
trans-prob
trans-dist
smdp-trans-dist
reward
transition-matrix
reward-matrix
state-set
avail-actions
action-set
terminal?
sparsify
num-states

tabular SMDPs
-------------
num-avail-actions-vec
term-vec
trans-vec

hierarchical SMDPs
------------------
level

Outcomes in transition distributions
------------------------------------
make-outcome
outcome-state
outcome-reward
outcome-duration
outcome-p
"))


(in-package cl-user)
	   
