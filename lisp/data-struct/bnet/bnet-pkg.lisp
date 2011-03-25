(defpackage bayes-net
  (:documentation "Package for Bayes nets.

Types
-----
<2tbn>
2bn-var-desc

Variable descriptions
---------------------
make-cpd
make-param-desc

2tbn operations
---------------
2tbns are [cond-prob-dist]s, so see the prob package for applicable functions.  Operations specific to 2tbns :
init-dist
reward
state-set
action-set

")
  
  (:nicknames bnet)
  (:use utils
	cl
	set
	prod-set
	inst-vars
	prob)
  (:export <2tbn>
	   make-2tbn
	   2tbn-var-desc
	   make-cpd
	   make-param-desc
	   reward
	   make-2tbn-var-desc
	   state-set
	   action-set
	   init-dist)
  )

(in-package bayes-net)