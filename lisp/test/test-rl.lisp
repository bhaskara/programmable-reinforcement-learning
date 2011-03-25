(defpackage test-rl
  (:use common-lisp
	utils
	rl-user
	q-learning
	td-taxi-env
	gold-standard))

(in-package test-rl)

(format t "~&Testing RL Code")
(fill-format-nl #\= 60)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create environment, compute ground truth
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf env (td-taxi-env:make-example-env1))
;(setf v-true (make-array 3 :initial-contents '(22.3 19.8 0)))
;;(setf q-true (dp:q-from-v mdp-test-envs:*test-mdp-1* v-true))
;(setf e (make-instance 'mdp-env:<mdp-env> :mdp mdp-test-envs:*test-mdp-1*
;			       :init-dist #(.5 .5)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; learning exp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun evaluate-alg (alg)
  (evaluate env (get-policy-hist alg) :num-steps 25 
	    :num-trials 10 :env-display-stream nil 
	    :pause-after-actions nil))





(setf *num-steps-learning* 20000)
(setf *hist-length* 100)

(setf featurizer (td-taxi-flat-lfa:make-taxi-featurizer env))
(setf lq
  (make-instance 'q-fn:<env-q-function> :env env :env-name 'env :featurizer featurizer
		 :featurizer-name 'featurizer
		 :fn-approx (make-instance 'fn-approx:<linear-fn-approx> :dim 7)))

(setf q-learner (make-instance '<q-learning> :env env :lrate .01 :discount 1))
(setf lfa-q-learner (make-instance '<q-learning> :lrate .01 :discount 1 :q-fn lq :hist-out-dir "test/temp"))
(setf api-learner (make-instance 'api:<approx-pol-it> :env env :pol-imp-int 100 :pol-switch 500))
(setf f nil)
;(with-outfile (f "scratch/test-gs.out")
(setf gs-learner (make-gold-standard-learning-algorithm :debug-str f))
(setf env-obs (env-observer:make-env-observer f))
(learn env 'random (list q-learner lfa-q-learner gs-learner) *num-steps-learning*
       :hist-length *hist-length* :ep-print-inc 10 :step-print-inc 100)
; (on-policy-learning e api-learner *num-steps-learning*
; 		    :hist-length *hist-length* :ep-print-inc 100 :step-print-inc 100)

(setf qh (get-q-hist q-learner))
(setf lqh (get-q-hist lfa-q-learner))
(setf gh (get-q-hist gs-learner))
; (setf ah (get-q-hist api-learner))
      



(format t "~&Q-learning algorithm learning curve is~&~W"
	(setf q-rews (evaluate-alg q-learner)))

(format t "~&Linearly approximated Q-learning learning curve is~&~W"
	(setf lq-rews (evaluate-alg lfa-q-learner)))

(format t "~&Gold standard algorithm learning curve is~&~W"
 	  (setf gs-rews (evaluate-alg gs-learner)))

; (format t "~&Approximate policy iteration learning curve is~&~W"
;	(setf api-rews (evaluate-alg api-learner)))


(reset lfa-q-learner)

(in-package common-lisp-user)    










