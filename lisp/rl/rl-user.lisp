;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rl/rl-user.lisp
;; contains functions to make it easier for users to call the reinforcement
;; learning code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package rl)


(defun learn (env policy obs num-steps &key (hist-length nil) (step-print-inc 100) (ep-print-inc 10))
  "learn ENV POLICY OBSERVERS NUM-STEPS &key (HIST-LENGTH nil) (STEP-PRINT-INC 100) (EP-PRINT-INC 10)

A frontend for running trajectory-based rl as defined in the <rl-control> class.  
ENV - type <env>
POLICY - Either an object of type [policy], or the symbol 'rl:random, which means choose uniformly at random from available actions.
OBSERVERS - either an <rl-observer> or a list of <rl-observers>.  <rl-observers> include learning algorithms as well as objects that gather statistics, etc.  
NUM-STEPS - Number of env steps to go for.

Keys
HIST-LENGTH - Length of history to gather.  Nil by default, which means don't gather history.
STEP-PRINT-INC - Print acknowledgement every so many env steps
EP-PRINT-INC - Print acknowledgement every so many episodes

Resets all learning algorithms by default."
  
  (when (eq policy 'random)
    (setf policy (policy:make-random-policy (lambda (s) (env:avail-actions env s)))))
  (unless (listp obs)
    (setf obs (list obs)))
  (dolist (o obs)
    (when (typep o '<learning-algorithm>)
      (reset o)
      (set-hist-collect 
       (make-evenly-spaced-intervals 1 num-steps hist-length)
       o)
      (when (typep o '<on-policy-learning-algorithm>)
	(assert (eq o policy) nil
	  "Observer ~a is an on-policy learning algorithm, but it does not equal the exploration policy ~a" o policy))))
    
  (push (progress-printer:make-progress-printer step-print-inc ep-print-inc)
	obs)
  (reinforcement-learning (make-instance '<rl-control>
				:env env
				:policy policy
				:obs obs
				:num-steps num-steps)))


(defun on-policy-learning (env alg &rest args)
  "on-policy-learning ENV ALG &rest ARGS.  Like calling (learn ENV ALG ALG &rest ARGS)."
  (apply #'learn env alg alg args))

(defun evaluate (env policies &key (num-steps nil) (step-print-inc 100) (env-display-stream nil) (pause-after-actions nil)
				   (num-trials 1))
  "evaluate ENV POLICIES &key (NUM-TRIALS 1) (NUM-STEPS nil) (STEP-PRINT-INC 100) (ENV-DISPLAY-STREAM nil) (PAUSE-AFTER-ACTIONS nil)

Front end for evaluating policies.  POLICIES is either a policy or a sequence of policies.  Do the following for each policy : reset environment to a random state, and run the given policy, computing the total reward, until either the episode terminates or NUM-STEPS environment steps happen.  Repeat this NUM-TRIALS times.   If POLICIES was a sequence, return a corresponding sequence of average total rewards rounded to 2 decimal places.  If POLICIES was a singleton, just return a number. Acknowledgement is printed every STEP-PRINT-INC steps, if it is non-nil.  If ENV-DISPLAY-STREAM is non-nil, then print what happens in environment to this stream.  If, in addition, PAUSE-AFTER-ACTIONS is non-nil, then ask user to press enter after each step.  Future todo : make it so a set of initial states is sampled, and those initial states are used to evaluate each policy."
  
  (let* ((pp (progress-printer:make-progress-printer step-print-inc nil))
	 (sg (stat-gatherer:make-stat-gatherer))
	 (obs (list sg pp)))

    
    (awhen env-display-stream
	   (push (env-observer:make-env-observer it pause-after-actions) obs))
    
    (when step-print-inc
      (format t "~&Evaluating policies"))
    (flet ((evaluate-policy (pol)
	     (let ((rlc (make-instance '<rl-control>
			  :env env
			  :policy pol
			  :obs obs
			  :num-steps num-steps
			  :num-episodes 1)))
	       (format t ".")
	       (round-decimal
		(/
		 (loop
		     for j below num-trials
		     do (reinforcement-learning rlc)
		     sum (stat-gatherer:total-reward sg))
		 num-trials)

		2))))

      (typecase policies
	(set:[numbered-set] (set:mapset 'vector #'evaluate-policy policies))
	(otherwise (evaluate-policy policies))))))
      


(defun io-interface (env &rest advisors)
  "io-interface ENV &rest ADVISORS.  Run in the environment, prompting user for actions.  At each state, each advisor will print a debugging message.  For example, q-functions will print q-values of each choice, policies will print the recommended choice, etc."
  (let ((pol 
	 (make-instance 'policy:<prompt-policy>
	   :choice-fn (lambda (s) (env:avail-actions env s))
	   :advisors advisors
	   :io-specifier env)))
    (reinforcement-learning (make-instance '<rl-control>
			      :env env
			      :policy pol
			      :obs (list (env-observer:make-env-observer))))))

  