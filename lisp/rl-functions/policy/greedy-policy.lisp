;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rl-functions/policy/greedy-policy.lisp
;; Defines <greedy-policy> class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package policy)

(defclass <greedy-policy> (<policy>)
  ((q-function :type q-fn:<q-function>
	       :reader q-function
	       :initarg :q-function)
   (random-choice :initform t
		  :reader random-choice
		  :initarg :random-choice))
  (:documentation "<greedy-policy>.  Subclass of <policy>.  

Initargs
:q-function - make decisions by maximizing this q-function.
:random-choice - This can be either 1) a function, such that if an unknown-state or unknown-state-action condition is encountered, then a choice is instead made by applying this function the current state, and choosing randomly from the resulting state. 2) nil, in which case errors are passed up to the caller 3) t (the default), in which case unknown-state errors are passed up, but unknown-state-action errors are handled by choosing randomly from the known set of available actions"))


(defmethod make-choice ((d <greedy-policy>) omega)
  (handler-bind ((q-fn:unknown-state-action
		  #'(lambda (c)
		      (when (random-choice d)
			(q-fn:choose-randomly c))))
		 (q-fn:unknown-state
		  #'(lambda (c)
		      (declare (ignore c))
		      (awhen (random-choice d)
			     (when (functionp it)
			       (use-value (prob:sample-uniformly (funcall it omega))))))))
    (q-fn:best-choice (q-function d) omega)))


(defmethod clone ((pol <greedy-policy>))
  (check-exact-class pol '<greedy-policy>)
  (make-instance '<greedy-policy> :q-function (clone (q-function pol)) :random-choice (random-choice pol)))

(defmethod print-object ((pol <greedy-policy>) str)
  (if *print-readably*
      (progn
	(check-exact-class pol '<greedy-policy>)
	(format str "#.(make-instance 'policy:<greedy-policy> :q-function ")
	(write (q-function pol) :stream str)
	(format str " :random-choice ~W)"))
    (call-next-method)))

		