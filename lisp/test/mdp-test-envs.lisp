;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mdp-test-envs.lisp
;; will hold some simple debugging examples for mdp code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage mdp-test-envs
  (:use common-lisp
	mdp
	maze-mdp
	utils)
  (:export *test-mdp-1*
	   *test-mdp-2*))

(in-package mdp-test-envs)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; *test-mdp-1*
;;
;; 3 states, 2 actions (clockwise and counter)
;;
;;            0
;;           / \
;;          /   \
;;         2-----1
;;
;; 2 is a terminal state.  no discounting
;; note that all policies are proper because of noisy transitions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *trans-mat-1*
  (make-array '(3 2 3)
	      :initial-contents 
	      '(((.2 .7 .1) (.3 .2 .5))
		((.1 .2 .7) (.5 .3 .2))
		((0 0 1) (0 0 1)))))

(defparameter *rew-mat-1*
  (make-array '(3 2 3)
	      :initial-contents
	      '(((2 5 1) (2 5 1))
		((2 5 1) (2 5 1))
		((0 0 0) (0 0 0)))))

(defparameter *test-mdp-1* (mdp:make-tabular-mdp *trans-mat-1* *rew-mat-1* :termination-vector #(nil nil t)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; *test-mdp-2*
;; maze-world created using create-maze-mdp
;; 
;; 2 W 4
;; 1 -2 -3
;; numbers denote reward, W is a wall, top right state is terminal
;;
;; discount .8, cost-of-living 5
;;
;; move-success-prob .8
;; collision-cost 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(defparameter *world-map* (make-array '(2 3) :initial-contents '((T nil T) (T T T))))
(defparameter *term* (make-array '(2 3) :initial-contents '((nil nil T) (nil nil nil))))
(defparameter *rew* (make-array '(2 3) :initial-contents '((2 0 4) (1 -2 -3))))

(defparameter *test-mdp-2* (make-maze-mdp *world-map* :rewards *rew* :cost-of-living 5 :move-success-prob .8 :termination *term* :collision-cost 2))

(in-package common-lisp-user)	      
   








