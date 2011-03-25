(defpackage reinforcement-learning
  (:documentation "
The learning package.  Contains generic code for doing RL in an environment, as opposed to specific algorithms.  

Operations
- learn-in-env
")
  (:nicknames rl)
  (:export learn-in-env))

(in-package reinforcement-learning)


(defconstant *env-reset-max* 200)


(defun learn-in-env (env algorithms num-steps hist-length &key (print-progress 100))
  "learn-in-env ENV ALGS NUM-STEPS HIST-LENGTH &KEY (PRINT-PROGRESS 100).  Use each algorithm in ALGS (which must be a list of elements of type <q-learning-alg>) to do reinforcement learning in the environment ENV.  All algorithms share the same samples (generated, for now, by random exploration) to make comparisons more fair.  NUM-STEPS steps are taken in the environment.  The return value is a list HISTS, containing, for each algorithm in ALGS, a history, which is an array of length HIST-LENGTH containing snapshots of the q-fn of that algorithm at regular intervals during learning.  In the special case of a single algorithm, ALGS can just be an object of type <q-learning-alg> in which case the return value will also be a history rather than a list containing the history.  PRINT-PROGRESS is how often to print a ``.'' indicating progress."
  (loop
      with algs = (if (listp algorithms) algorithms (list algorithms))
      with inc = (if (= hist-length 1) (1+ num-steps) (floor (/ (1- num-steps) (1- hist-length))))
      with hist = (loop
		      repeat (length algs)
		      collect (make-array hist-length :fill-pointer 0))
      with return-list = (listp algorithms)
		  
      with print-inc = (if (= print-progress 0) (1+ num-steps)
			 (ceiling (/ num-steps print-progress)))
		  
      initially (env:reset env)
		(dolist (alg algs)
		  (ql-alg:reset alg))

		
      for step from 1 to num-steps
			 
			 
      ;;; reset if we're at a terminal state		 
      do (loop while (env:at-terminal-state env)
	     for i from 0
	     do (env:reset env)
		(assert (< i *env-reset-max*) nil "Tried ~a times and was unable to reset to a nonterminal state" *env-reset-max*))
	
	
	 (let* ((s (env:get-state env))
		(act (env:get-actions env))
		(a (aref act (random (length act)))))
	   (multiple-value-bind (r s2)
	       (env:do-action env a)
	     (dolist (alg algs)
	       (ql-alg:observe alg s a r s2))))
	 
      when (= 0 (mod (- num-steps step) inc))
      do (mapcar (lambda (alg h)
		   (vector-push (ql-alg:get-q-fn alg) h))
		 algs hist)
	 
      when (= 0 (mod step print-inc))
      do (format t ".")
		
      finally (when (> print-progress 0) (format t "~%"))
	      (return (if return-list hist (first hist)))))






(in-package cl-user)
  
  
	   
   