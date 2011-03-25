;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rl-functions/policy/prompt-policy.lisp
;; TODO : this class is getting unwieldy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package policy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; class def, constructor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <prompt-policy> (<policy>)
  ((advisors :type list :reader advisors :initarg :advisors)
   (choice-fn :type function :reader choice-fn :initarg :choice-fn)
   (io-specifier :initarg :io-specifier :reader io-specifier))
  
  (:documentation "A <prompt-policy> is one which makes choices by asking the user.  It also has a list of 'advisors', which can print their advice before the user enters a choice.

Initargs
:choice-fn - Function that is called on a state and returns set of available choices
:advisors - list of advisors whose advice will be printed at each choice (see print-advice)
:io-specifier - An object that governs how the IO is done when asking the user to enter a choice.  Nil by default, which amounts to just printing the state and set of choices, and using the Lisp reader to interpret what the user enters as an action.  This will work fine in almost all situations.  Modifying this behaviour requires providing a method for prompt-for-choice"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; special variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *max-num-choices-to-print* 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro do-upto-max-choices ((choice-var) &body body)
  "do-upto-max-choices (CHOICE-VAR) &rest BODY.  Do BODY once for each choice, upto *max-num-choices-to-print*"
  ;; TODO this should be replaced by a pprint method
  (with-gensyms (num-printed)
    `(set:do-elements (,choice-var choices nil ,num-printed)
       (when (>= ,num-printed *max-num-choices-to-print*)        
	 (format str "~&And ~a other choices not shown" (- (set:size choices) ,num-printed))
	 (return))
       ,@body)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; implemented methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod make-choice ((pol <prompt-policy>) s)
  (let ((choices (funcall (choice-fn pol) s)))
    (format t "~&~%Currently at: ~&~W" s)
    (format t "~&Set of available choices is ~a" choices)
    (format t "~&~50@{~A~:*~}" #\-)
    (loop
	for a in (advisors pol)
	for i from 0
	do (format t "~&Advisor ~a: " i)
	   (print-advice a s choices t)
	   (fill-format-nl #\- 50))
    
    (prompt-for-choice s choices (io-specifier pol))
    ))

(defgeneric prompt-for-choice (s choices io-spec)
  (:documentation "prompt-for-choice STATE CHOICES IO-SPEC.  A way to customize the IO behaviour of prompt for choice.  Methods for this generic function are expected to prompt the user for a choice, parse the entered choice, and return it.  The process may involve a back-and-forth interaction with the user.  The method may also signal a 'choose-to-abort error if the user enters some value that signifies that the io-interface should finish.  

The default method 
- asks the user to enter the choice
- parses it using the lisp read function
- If it equals nil, aborts
- If it's not a member of the choice set, and if the set of choices has size 1, then prints a warning message and returns that single member.
- If it's not a member of the choice set, and if the set of choices has size greater than 1, asserts and allows the user to enter another choice.
- Finally returns the choice.

IO-SPEC receives standard values in various settings:
- rl:io-interface uses a prompt policy with io-spec being the environment
- alisp:io-interface and calisp:io-interface use a prompt policy with io-spec being the partial program.

")
  (:method (s choices io-spec)
	   (declare (ignore s io-spec))
	   (format t "~&Please enter choice, or nil to terminate. ")
	   ;; if one of the choices is nil, you're out of luck...
	   (let ((ch (read))
		 is-member reason)
	     (assert
		 (progn
		   (unless ch
		     (error 'choose-to-abort))
		   (if (eql (set:size choices) 1)
		       (progn
			 (unless (set:member? ch choices)
			   (warn "~a is not a valid choice, but using default value ~a instead."
				 ch (set:item 0 choices))
			   (setf ch (set:item 0 choices)))
			 (setf is-member t))
		     (multiple-value-setq (is-member reason)
		       (set:member? ch choices)))
		   is-member)
		 (ch)
	       (set:get-full-explanation-string reason))
	     ch)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; advice generic function and some common methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric print-advice (advisor omega choices str)
  (:documentation "print-advice ADVISOR STATE CHOICES STREAM.  Given that we are at STATE and have choices CHOICES, print ADVISOR's opinion to STREAM."))

(defmethod print-advice ((adv <policy>) omega choices str)
  (declare (ignore choices))
  (format str "Policy recommends ~a" (make-choice adv omega)))




(defmethod print-advice ((adv mdp:<mdp>) omega choices str)
  ;; a mdp prints its transition distribution
  (format t "Transition distributions are ")
  (set:do-elements (u choices)
    (format str "~&~a" (mdp:trans-dist adv omega u))))


(defmethod print-advice ((adv function) omega choices str)
  (let ((l (get-lambda-list adv)))
    (ecase (length l)
      (2 (print-state-action-feature-advice adv omega choices str))
      (1 (let ((result (funcall adv omega)))
	   (if (set:member? result choices)
	       ;; treat adv as a policy
	       (format str "Policy recommends ~a" result)
	     (print-cond-dist-advice adv omega choices str)))))))

(defun print-cond-dist-advice (adv omega choices str)
  (format str "Function of one argument being treated as a conditional distribution by prompt-policy.")
  (set:do-elements (u choices)
    (format str "~&When making choice ~a" u)
    (prob:do-sample-points (omega2 prob (cond-dist adv (cons omega u)))
      (format str "~&Item ~a with probability ~a" omega2 prob))))

(defun print-state-action-feature-advice (adv omega choices str)
  (format str "Featurizer.")
  (do-upto-max-choices (u)
    (format str "~&Choice ~a : features ~a" u (funcall adv omega u))))
    
  
		