(defpackage test-calisp
  (:use cl
	rbe
	calisp-user
	prob
	thread-dec-q
	rbe-dec
	utils
	rbe-prog
	rbe-linear-features
	inst-vars))


(in-package test-calisp)


(setf num-peas 2)
(setf wm (make-array '(1 4) :initial-element 't))
(setf base-loc '(0 1))
(setf forest-locs '((0 0)))
(setf mine-locs '((0 3)))
(setf max-gold 2
      max-wood 2)


(setf e (make-rbe-env wm num-peas max-gold max-wood base-loc forest-locs mine-locs))
(setf p (make-rbe-prog))
(setf q (make-rbe-crl-q-function max-gold max-wood)
      q-dec (make-rbe-dec-q-function max-gold max-wood num-peas))
(setf (crlq:debug-mode q) t)



(format t "~&This should take about 100 '.'s~%")
;(calisp:io-interface p e nil)

(setf tq-learner (make-instance '<threadwise-decomposed-q-learner>
		   :q-function q-dec
		   :reward-decomposer #'rbe-reward-decomp))
(setf sq (make-instance 'csq:<smdpq> :q-fn q))
(calisp-user:learn p e 'random (list sq tq-learner) 5000 :hist-length 50 :step-print-inc 100 :episode-print-inc 10)
(setf sq-q-hist (get-q-hist sq)
      sq-pol-hist (get-policy-hist sq)
      tq-q-hist (get-q-hist tq-learner)
      tq-pol-hist (get-policy-hist tq-learner)
      sq-rews (calisp-user:evaluate p e sq-pol-hist :num-steps 50 :num-trials 1)
      tq-rews (calisp-user:evaluate p e tq-pol-hist :num-steps 50 :num-trials 1))
(pprint sq-rews)
(pprint tq-rews)
	