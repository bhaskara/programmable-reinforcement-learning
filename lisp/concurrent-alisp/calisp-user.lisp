;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; calisp-user.lisp
;; contains functions that provide various ways for users to run concurrent 
;; alisp programs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package calisp)


(defvar *io-interface-debug-threads* t
  "T by default for now.  If true, then running the io-interface also prints low-level information about thread-related events to standard output.")

(defparameter *calisp-random-policy* (pol:make-random-policy #'js-choices))



(defun io-interface (part-prog env &optional (advisors nil) (algs nil))
  "io-interface PART-PROG ENV &optional (ADVISORS nil) (ALG nil)

PART-PROG : concurrent ALisp program
ENV : environment
ADVISORS : designator for a list of advisors
OBS : designator for a list of observers

Run this partial program in the environment in a mode where events in the program are printed and choices are made by asking the user.  For any of the observers in OBS that are also learning algorithms, set their debug stream to *standard-output* beforehand.  Store history in *hist*"
  (let ((pol (make-instance 'policy:<prompt-policy>
	       :choice-fn #'js-choices
	       :advisors (designated-list advisors)
	       :io-specifier part-prog))
	(deb-obs (when *io-interface-debug-threads* (make-instance '<thread-debugger>)))
	(alg-list (designated-list algs)))
    
    (let ((old-vals (mapcar 
		     #'(lambda (obs) (when (typep obs '<calisp-learning-algorithm>) (debug-str obs)))
		     alg-list)))
			      
      (dolist (alg alg-list)
	(when (typep alg '<calisp-learning-algorithm>)
	  (calisp-user:set-debug-str *standard-output* alg)))
      (push (make-instance '<calisp-io-int-observer>) alg-list)
      (run (make-instance '<crlm> :env env :part-prog part-prog :observers alg-list
			  :debugging-observers deb-obs :policy pol
			  :num-steps nil :num-episodes nil))
      (mapcar #'(lambda (alg old-val) (when old-val (calisp-user:set-debug-str old-val alg)))
	      (rest alg-list) old-vals)
      (values))))





(defun learn (part-prog env policy obs num-steps &key (hist-length nil) (episode-print-inc 10) (step-print-inc 100))
  "learn PART-PROG ENV POLICY OBSERVER-LIST NUM-STEPS &key (HIST-LENGTH nil) (EPISODE-PRINT-INC 10) (STEP-PRINT-INC 100).  Run ALisp partial program in environment using POLICY to make decisions.  POLICY can also be the symbol 'random for random choices.  Note that the learning algorithms are not reset beforehand - if you want them to start from scratch, call reset on them first."
  (setf policy
    (case policy
      (random *calisp-random-policy*)
      (otherwise policy)))
  
  (unless (listp obs)
    (setf obs (list obs)))
  
  (dolist (o obs)
    (when (typep o '<learning-algorithm>)
      (rl-user:set-hist-collect
       (make-evenly-spaced-intervals 1 num-steps hist-length)
       o)))
  
  (push (progress-printer:make-progress-printer step-print-inc episode-print-inc) obs)
  
  (when (or episode-print-inc step-print-inc)
    (format t "~&Learning"))
  
  (let ((c (make-instance '<crlm> :env env :part-prog part-prog :observers obs :policy policy
			  :num-steps num-steps)))
    (run c)))


(defun on-policy-learning (part-prog env alg &rest args)
  "on-policy-learning PART-PROG ENV ALG &rest ARGS.  Like calling (learn PART-PROG ENV ALG ALG &rest ARGS)."
  (apply #'learn part-prog env alg alg args))



(defun evaluate (part-prog env policies &key (num-steps nil) (step-print-inc 100)   (num-trials 1))
  "evaluate PART-PROG ENV POLICIES &key (NUM-TRIALS 1) (NUM-STEPS nil) (STEP-PRINT-INC 100) 

Front end for evaluating completions of a CALisp partial program, represnted as policies.  POLICIES is either a policy or a numbered set of policies.  Do the following for each policy : reset environment to a random state, and run the given policy, computing the total reward, until either the episode terminates or NUM-STEPS environment steps happen.  Repeat this NUM-TRIALS times.   If POLICIES is a <numbered-set>, return a corresponding sequence of average total rewards rounded to 2 decimal places.  If POLICIES is not a numbered-set, just return a number. Acknowledgement is printed every STEP-PRINT-INC steps, if it is non-nil.   Future todo : make it so a set of initial states is sampled, and those initial states are used to evaluate each policy."
  
  
  (when step-print-inc (format t "~&Evaluating policies"))
  (let* ((sg (stat-gatherer:make-stat-gatherer))
	 (obs (list sg (progress-printer:make-progress-printer step-print-inc nil))))
    (flet ((evaluate-policy (p)
	     (let ((c (make-instance '<crlm>
			 :env env
			 :policy p
			 :part-prog part-prog
			 :observers obs
			 :num-steps num-steps
			 :num-episodes 1)))
	       (format t ".")
	       (round-decimal
		(/
		 (loop
		     repeat num-trials
		     do (run c)
		     sum (stat-gatherer:total-reward sg))
		 num-trials)
		2))))
      
      ;; main function starts here
      (typecase policies
	(set:[numbered-set] (set:mapset 'vector #'evaluate-policy policies))
	(otherwise (evaluate-policy policies))))))



