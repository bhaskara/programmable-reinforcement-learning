;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rl-functions/policy/random-policy.lisp
;; Defines <random-policy> class
;; TODO : make this a subclass of exploration-policy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package policy)


(defclass <random-policy> (<policy>)
  ((choice-fn :type function :reader choice-fn :initarg :choice-fn :writer set-choice-fn))
  (:documentation "A policy that chooses uniformly at random from the actions available at any state.  Assumes the environment implements avail-actions, and that the action set is small enough that we can sample by simple enumeration.

Initargs
:choice-fn - function that maps state to set of available choices
:env - environment.  Must be provided if choice-fn is not.
"))


(defmethod initialize-instance :after ((pol <random-policy>) &rest args &key env)
  (unless (slot-boundp pol 'choice-fn)
    (set-choice-fn (lambda (s) (env:avail-actions env s)) pol)))

(defun make-random-policy (choice-fn)
  "make-random-policy CHOICE-FN.  Make a policy of type <random-policy> that chooses randomly from the set of actions available at a given state."
  (make-instance '<random-policy> :choice-fn choice-fn))


(defmethod make-choice ((d <random-policy>) s)
  (let ((choices (funcall (choice-fn d) s)))
    (set:item (random (set:size choices)) choices)))

		

		