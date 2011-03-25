;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; test-mdp.lisp
;;
;; Testing for the basic mdp and dp code.  
;;
;; Tests 
;; - mdp creation, single-variable for state, action, using make-tabular-discounted-mdp and make-tabular-sssp
;; - value iteration, policy iteration, policy evaluation
;; - create-maze-mdp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Package info
(defpackage test-mdp
  (:use common-lisp
	dp
	mdp
	maze-mdp
	grid-world
	lin-alg
	utils
	mdp-test-envs))


(in-package test-mdp)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic tests for mdp creation, dp algs on 3-state mdps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(format t "~&Testing MDP code")
(fill-format-nl #\= 60)

(format t "~&Performing tests on *test-mdp-1* without discount")
(fill-format-nl #\- 50)
(format t "~&Value iteration")
(setf v (value-iteration *test-mdp-1*))
(setf v-true (value-fn:make-tabular-value-fn #(22.3 19.8 0)))
(assert (< (lp-dist v v-true) .1) nil "Incorrect answer for value-iteration for *test-mdp-1*")

(format t "~&Policy iteration")
(setf pol (policy-iteration *test-mdp-1* :epsilon .1))

(format t "~&Modified policy iteration")
(setf mpi (policy-iteration *test-mdp-1* :k 10))

(setf pi-true (policy:make-tabular-policy #(0 1 0)))
(assert (same pol pi-true) nil "Incorrect answer for policy iteration for *test-mdp-1*")
(assert (same mpi pi-true) nil "Incorrect answer for modified policy iteration for *test-mdp-1*")


(format t "~&Policy evaluation")
(setf vpi (policy-evaluation *test-mdp-1* (policy:make-tabular-policy #(1 1 1))))
(setf vpi-true (value-fn:make-tabular-value-fn #(5.15 7.53 0)))
(assert (< (lp-dist vpi-true vpi) .1) nil "Incorrect answer for policy evaluation for *test-mdp-1*")


(setf disc .7)

(format t "~&~%Performing tests on *test-mdp-1* with discount")
(fill-format-nl #\- 50)
(format t "~&Value iteration")
(setf v2 (value-iteration *test-mdp-1* :discount disc))
(setf v2-true (value-fn:make-tabular-value-fn #(8.82 7.33 0)))
(assert (< (lp-dist v2 v2-true) 0.1) nil "Incorrect answer for value iteration for *test-mdp-1*")


(format t "~&Q from V")
(setf q2 (q-from-v *test-mdp-1* v2-true disc))
(setf q2-true (q-fn:make-tabular-q-function #(#(8.8265 4.9784) #(3.5436 7.3263) #(0 0))))
(assert (< (lp-dist q2 q2-true) 0.1) nil "Incorrect answer for q-from-v for *test-mdp-1*")


(format t "~&Policy iteration")
(setf pi2 (policy-iteration *test-mdp-1* :epsilon .1 :discount disc))
(setf pi2-true (policy:make-tabular-policy #(0 1 0)))
(assert (same pi2 pi2-true) nil "Incorrect answer for policy iteration for *test-mdp-1*")

(format t "~&Modified policy iteration")
(setf mpi2 (policy-iteration *test-mdp-1* :k 10 :discount disc))
(assert (same mpi2 pi2-true) nil "Incorrect answer for modified policy iteration for *test-mdp-1*")

(format t "~&Policy evaluation")
(setf vpi2 (policy-evaluation *test-mdp-1* (policy:make-tabular-policy #(1 1 1)) :discount disc))
(setf vpi2-true (value-fn:make-tabular-value-fn #(3.54 4.98 0)))
(assert (< (lp-dist vpi2 vpi2-true) 0.1) nil "Incorrect answer for policy evaluation for *test-mdp-1*")




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; a bigger example using create-maze-mdp, that has multiple state variables.  tests that the 
;; trans-matrix, reward, etc are correctly created and that value and policy iteration give the
;; right answers.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





(setf disc .8)

(format t "~&~%Performing tests on *test-mdp-2* with discount")
(fill-format-nl #\- 50)

(format t "~&Verifying transition matrix")
(setf tr3-true (make-array '(6 5 6)
			   :initial-contents 
			   '(((1 0 0 0 0 0) (0.9 0 0 0.1 0 0) (0.2 0 0 0.8 0 0) (0.9 0 0 0.1 0 0) (1 0 0 0 0 0))
			     ((0.1 0.8 0.1 0 0 0) (0 0.1 0.8 0 0.1 0) (0.1 0.0 0.1 0 0.8 0) (0.8 0.1 0 0 0.1 0) (0 1 0 0 0 0))
			     ((0 0 1 0 0 0) (0 0 1 0 0 0) (0 0 1 0 0 0) (0 0 1 0 0 0) (0 0 1 0 0 0))
			     ((0.8 0 0 0.1 0.1 0) (0.1 0 0 0.1 0.8 0) (0 0 0 0.9 0.1 0) (0.1 0 0 0.9 0 0) (0 0 0 1 0 0))
			     ((0 0 0 0.1 0.8 0.1) (0 0 0 0 0.2 0.8) (0 0 0 0.1 0.8 0.1) (0 0 0 0.8 0.2 0) (0 0 0 0 1 0))
			     ((0 0 0.8 0 0.1 0.1) (0 0 0.1 0 0 0.9) (0 0 0 0 0.1 0.9) (0 0 0.1 0 0.8 0.1) (0 0 0 0 0 1)))))
(setf tr3 (mdp:transition-matrix *test-mdp-2*))
(assert (< (lp-dist tr3 tr3-true) .1) nil "Incorrect transition matrix ~a for *test-mdp-2* - truth was ~a" tr3 tr3-true)
(format t "~&Verifying reward matrix")
(setf r3-true (make-array '(6 5 6)
			  :initial-contents
			  '(((-7 0 0 0 0 0) (-7 0 0 -4 0 0) (-7 0 0 -4 0 0) (-7 0 0 -4 0 0) (-5 0 0 0 0 0))
			    ((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0))
			    ((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0))
			    ((-3 0 0 -7 -7 0) (-3 0 0 -7 -7 0) (0 0 0 -7 -7 0) (-3 0 0 -7 0 0) (0 0 0 -5 0 0))
			    ((0 0 0 -4 -7 -8) (0 0 0 0 -7 -8) (0 0 0 -4 -7 -8) (0 0 0 -4 -7 0) (0 0 0 0 -5 0))
			    ((0 0 -1 0 -7 -7) (0 0 -1 0 0 -7) (0 0 0 0 -7 -7) (0 0 -1 0 -7 -7) (0 0 0 0 0 -5)))))
(setf r3 (mdp:reward-matrix *test-mdp-2*))
(assert (< (lp-dist r3 r3-true) .1) nil "Incorrect reward matrix for *test-mdp-2*")
(setf states-3 '((0 0) (0 1) (0 2) (1 0) (1 1) (1 2)))

(format t "~&Value iteration")
(setf v3 (value-iteration *test-mdp-2* :discount disc))
(setf v3-true (value-fn:make-tabular-value-fn #(-18.47 0 0 -17.06 -11.9 -3.4)))
(assert (< (lp-dist v3 v3-true) .1) nil "Incorrect result of value iteration for *test-mdp-2*")

(format t "~&Policy iteration")
(setf pi3 (policy-iteration *test-mdp-2* :epsilon .1 :discount disc))
(format t "~&Modified policy iteration")
(setf mpi3 (policy-iteration *test-mdp-2* :k 10 :discount disc))
(setf pi3-true (policy:make-tabular-policy #(S R N E E N) (mdp:state-set *test-mdp-2*) ))
(assert (same pi3-true pi3) nil "Error in policy iteration for *test-mdp-2*")
(assert (same pi3-true mpi3) nil "Error in modified policy iteration for *test-mdp-2*")



(format t "~%~%All tests successfully completed.~%")


(in-package common-lisp-user)    










