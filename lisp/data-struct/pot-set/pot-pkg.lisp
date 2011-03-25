(defpackage pot-set
  (:documentation "Package pot-set - contains code for potential sets

Types
-----
potential
tabular-potential

Constructors
------------
make-function-potential

Binary operations for potential sets
------------------------------------
*times*
*plus*
*max*

Operations on potentials
------------------------
vars
eval-pot
multiply
multiply-and-sum-out
compose-potential

Operations on sets of potentials
--------------------------------
best-assignment
boltzmann-gm
min-deficiency-elim-order

Other symbols
-------------
not-used
uninstantiated
")
   
   
  
  (:use 
   cl
   utils
   inst-vars
   prob
   prod-set
   set)
  (:export
   potential
   tabular-potential
   make-function-potential
   
   *times*
   *plus*
   *max*
   *min*

   vars
   eval-pot
   multiply
   multiply-and-sum-out
   compose-potential
   
   best-assignment
   boltzmann-gm
   min-deficiency-elim-order
   
   not-used
   uninstantiated))
   



(in-package cl-user)