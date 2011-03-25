(defpackage reinforcement-learning
  (:documentation "Internal package for (flat) reinforcement learning.
")
  (:nicknames rl)
  (:use common-lisp
	utils
	prob)
)

(defpackage rl-user
  (:documentation "Package rl-user.  Package used when running rl algorithms in environments.

Exported symbols
==================

Running
-------
- learn
- on-policy-learning
- evaluate  
- io-interface
- random

General operations on learning algorithms
-----------------------------------------
- hist
- get-policy-hist
- get-q-hist
- get-value-fn-hist
- get-mdp-hist
- set-hist-collect
- reset
- debug-str
- set-debug-str

Making some common learning algorithms
--------------------------------------
- make-q-learning-alg


Related packages
================

Other observers 
---------------
- env-observer
- progress-printer
- q-learning
- gold-standard
- stat-gatherer
")
  (:use common-lisp
	package-utils))

(in-package rl-user)

(export-from 'reinforcement-learning
	     "learn"
	     "on-policy-learning"
	     "evaluate"
	     "io-interface"
	     "random"
	     "hist"
	     "set-hist-collect"
	     "get-policy-hist"
	     "get-value-fn-hist"
	     "get-q-hist"
	     "get-mdp-hist"
	     "make-q-learning-alg"
	     "reset"
	     "debug-str"
	     "set-debug-str")

	     

(in-package cl-user)

(defpackage rl-observer
  (:nicknames rl-obs)
  (:documentation "Package rl-observer.  Used when making new observers/learning algorithms/stats gatherers.

Types
-----
<rl-observer>
<learning-algorithm>
<on-policy-learning-algorithm>
<q-learning-algorithm>
<policy-learning-algorithm>
<value-learning-algorithm>

Macro
-----
defmessage
debug-msg

Messages
--------
inform-start-execution
inform-finish-execution
inform-start-episode
inform-env-step

Learning algorithms must implement some subset of (depending on specific type)
------------------------------------------------------------------------------
reset
knowledge-state
get-q-fn (q-learning algorithms)
get-mdp (model-learning algorithms)
get-policy (policy-learning algorithms)
get-value-fn (value-learning algorithms)
make-choice (on-policy algorithms)

Functions for use by learning algorithms
----------------------------------------
current-env-step
current-episode-step

Other accessors for learning algorithms
---------------------------------------
debug-str
set-debug-str

")
  (:use common-lisp
	package-utils))

(in-package rl-observer)

(export-from 'reinforcement-learning
	     "<rl-observer>"
	     "<learning-algorithm>"
	     "<on-policy-learning-algorithm>"
	     "<q-learning-algorithm>"
	     "<policy-learning-algorithm>"
	     "<value-learning-algorithm>"
	     "defmessage"
	     "debug-msg"
	     "inform-start-execution"
	     "inform-finish-execution"
	     "inform-start-episode"
	     "inform-env-step"
	     "reset"
	     "knowledge-state"
	     "get-q-fn"
	     "get-policy"
	     "get-mdp"
	     "get-value-fn"
	     "current-env-step"
	     "current-episode-step"
	     "debug-str"
	     "set-debug-str"
	     )

(export-from 'policy
	     "make-choice")


(in-package cl-user)
	     
