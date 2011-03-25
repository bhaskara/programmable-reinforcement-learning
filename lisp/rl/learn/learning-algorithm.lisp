;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rl/learn/learning-algorithm.lisp - Defines the <learning-algorithm> subclass
;; of <rl-observer>, and various subclasses of it
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package rl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; class def
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <learning-algorithm> (<rl-observer>)
  
  ((history :reader hist :accessor alg-hist :initform (make-array 0 :adjustable t :fill-pointer 0))
   (hist-out-dir :reader hist-out-dir :initarg :hist-out-dir :initform nil)
   (hist-collect-pred :reader hist-collect-pred :writer set-hist-collect-pred :type function)
   (env-steps :accessor env-steps :reader current-env-step)
   (num-hist-files :accessor num-hist-files :initform 0)
   (episode-steps :accessor episode-steps :reader current-episode-step)
   (debug-str :initarg :debug-str :reader debug-str :writer set-debug-str :initform nil))
  
  (:documentation "A <learning-algorithm> is just a particular type of <rl-observer> that learns a policy (or Q-function or model) from experience in the environment.

Initargs
--------
:hist-collect.  This can either be a number, in which case the current state of knowledge is saved every so many steps, or a predicate on fixnums, in which case the predicate is applied to the number of elapsed steps, and the state is saved whenever it returns true, or nil, in which case no history is saved.
:debug-str.  Stream to which debug messages are sent.
:hist-dir.  This is nil by default, which means the history is stored in memory.  If, instead, it is a string, then the history is output to files in a temporary directory by that name.  If the directory already exists, it is overwritten.  The history files are deleted when the function reset is called on the learning algorithm - if it is not called before Lisp is exited, then the temporary files will *not* be removed.  For the file version to work, the knowledge state of the algorithm must be an object which can be printed readably, i.e., it and all its components have print-object methods that behave correctly w.r.t the *print-readably* variable.  

History is automatically saved according to hist-collect-pred, and reset every time a inform-start-execution message is received.

Subclasses of <learning-algorithm> include <policy-learning-algorithm>, <value-learning-algorithm>, <q-learning-algorithm>, <model-learning-algorithm>.

Subclasses of these must implement whatever methods they need from <rl-observer>.  In addition, they must implement reset,
knowledge-state and any methods from their parent (e.g. get-q-fn for subclasses of <q-learning-algorithm>.   Given these, they don't need to worry managing the history."))

(defmethod initialize-instance :after ((alg <learning-algorithm>) &rest args &key hist-collect)
  (declare (ignore args))
  (set-hist-collect hist-collect alg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; implemented methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric reset (alg)
  (:documentation "reset ALG.  Reset the state of this algorithm, so it forgets everything that has been learnt.  There is a top-level :before method that resets the history.  There is an around method that makes ALG be returned.")
  (:method :before ((alg <learning-algorithm>))
	   
	   (awhen (hist-out-dir alg)
	     (dotimes (i (num-hist-files alg))
	       (delete-file (format nil "~a/~a" it i))
	       ))
	   (setf (alg-hist alg) (make-array 0 :adjustable t :fill-pointer 0)
		 (num-hist-files alg) 0
		 (env-steps alg) 0
		 (episode-steps alg) 0))
  (:method :around ((alg <learning-algorithm>))
	   (progn
	     (call-next-method)
	     alg)))

(defmethod inform-start-execution :before ((alg <learning-algorithm>))
  (awhen (hist-out-dir alg)
    (ensure-directories-exist (concatenate 'string it "/dummy")))
  (setf (env-steps alg) 0
	(episode-steps alg) 0))
  

(defmethod inform-env-step :before ((alg <learning-algorithm>) act rew to term)
  (declare (ignore to act rew term))
  (incf (env-steps alg))
  (incf (episode-steps alg)))

(defmethod inform-env-step :after ((alg <learning-algorithm>) act rew to term)
  (declare (ignore to act rew term))
  (when (funcall (hist-collect-pred alg) (env-steps alg))
    (save-state alg)))

(defmethod inform-start-episode :before ((alg <learning-algorithm>) s)
  (declare (ignore s))
  (setf (episode-steps alg) 0))

(defun save-state (alg)
  (aif (hist-out-dir alg)
      (with-outfile (f (format nil "~a/~a" it (num-hist-files alg)))
	(with-standard-io-syntax 
	  (let (
		#+allegro (excl:*print-nickname* t)
		)
	      (write (knowledge-state alg nil) :stream f)))
	(incf (num-hist-files alg)))
    (vector-push-extend (knowledge-state alg t) (alg-hist alg))))


(defun set-hist-collect (hist-collect alg)
  "set-hist-collect HC ALG.  If HC is null, ALG never collects history.  If it's a fixnum, collect history every HC steps.  If it's a function of one argument, collect history iff that function applied to the current number of env steps returns true."
  (etypecase hist-collect
    (null (set-hist-collect-pred (constantly nil) alg))
    (fixnum (set-hist-collect-pred (lambda (s) (eql 0 (mod s hist-collect))) alg))
    (function (set-hist-collect-pred hist-collect alg))))



(defmacro debug-msg (alg &rest args)
  "macro debug-msg ALG &rest ARGS.  Code that, when alg has a non-nil debug stream, calls format on it with ARGS."
  `(awhen (debug-str ,alg)
	  (format it ,@args)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; to be implemented by children
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric knowledge-state (alg &optional fresh)
  (:documentation "knowledge-state LEARNING-ALG &optional (FRESH t).  Return the current state of knowledge of the algorithm.  What exactly this is depends on the algorithm - Q-learning algorithms may return a Q-function, model-based learning algorithms may return a value function and transition model, and so on.  However, for any control-learning algorithm, the return value of this method should be something which can be turned into a policy using get-policy."))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; defn macro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro def-hist-extractor (fn-name extractor-name doc-string)
  "Macro for defining functions that do something to every item in the history."
  (with-gensyms (hist-item alg)
    `(defun ,fn-name (,alg)
       ,doc-string
       (map-history #'(lambda (,hist-item) (,extractor-name ,alg ,hist-item)) ,alg))))


(defun map-history (fn alg)
  "map-history FUNCTION LEARNING-ALGORITHM.  FUNCTION must be a function of a single argument.  Repeatedly apply it to each element of the history of knowledge-states of the algorithm, and store the results in a vector, which is returned."
  (aif (hist-out-dir alg)
      
      ;; if history is stored in files
      (set:make-image-set 
       (make-instance 'set:<directory-set>
	 :dir it :num-files (num-hist-files alg))
       fn)

    ;; if history is stored in memory
    (map 'vector fn (hist alg))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; on-policy learning
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <on-policy-learning-algorithm> (<learning-algorithm> policy:<policy>)
  ()
  (:documentation "An <on-policy-learning-algorithm> inherits from <learning-algorithm> and <policy>.  If this is used for learning, it must also be used as the exploration policy during learning. "))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; policy learning algorithms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <policy-learning-algorithm> (<learning-algorithm>)
  ()
  (:documentation "A <policy-learning-algorithm> is a kind of <learning-algorithm> that learns (among other things, perhaps), a policy).  Subclasses must implement the method get-policy.

Functions provided
- get-policy-hist - return history of policies"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; implemented here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-hist-extractor get-policy-hist get-policy 
  "get-policy-hist POLICY-LEARNING-ALG.  Take the history of this ALG and convert each item into a policy, and return the answers, in a <numbered-set> object.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; to implement by subclasses
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric get-policy (alg ks)
  (:documentation "current-policy POLICY-LEARNING-ALG KNOWLEDGE-STATE.  Turn this knowledge-state into a policy that can be run."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; value-learning algorithms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <value-learning-algorithm> (<learning-algorithm>)
  ()
  (:documentation "A <value-learning-algorithm> is a <learning-algorithm> that (among other things, perhaps), learns a value function.  Subclasses must implement get-value-fn.  

Provided functions
get-value-fn-hist"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; implemented here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-hist-extractor get-value-fn-hist get-value-fn
  "get-value-fn-hist VALUE-FN-LEARNING-ALG.  Take history of this algorithm and convert each item into a value function, and return them in a <numbered-set>.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; subclasses must implement
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric get-value-fn (alg ks)
  (:documentation "get-value-fn VALUE-LEARNING-ALG KNOWLEDGE-STATE.  Extract value function estimate from knowledge state."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Class <model-learning-algorithm>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <model-learning-algorithm> (<learning-algorithm>)
  ()
  (:documentation "A <model-learning-algorithm> is a <learning-algorithm> that learns a model of the environment, represented as an <mdp>.  Subclasses must implement get-mdp.

Provided functions
get-mdp-hist"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; implemented here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-hist-extractor get-mdp-hist get-mdp
  "get-mdp-hist MODEL-LEARNING-ALG.  Take history of this algorithm and extract learned model from each history item, and return the items, in a <numbered-set>.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; subclasses must implement
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric get-mdp (alg ks)
  (:documentation "get-mdp MODEL-LEARNING-ALG KNOWLEDGE-STATE.  Extract estimate of model (represented as an MDP) from knowledge state."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Class <q-learning-algorithm>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <q-learning-algorithm> (<value-learning-algorithm> <policy-learning-algorithm>)
  ()
  (:documentation "A <q-learning-algorithm> specializes both <value-learning-algorithm> and <policy-learning-algorithm> andlearns, among other things, a Q (action-value) function.  Subclasses must implement get-q-fn.  get-policy is implemented here (just act greedily according to the Q-function). 

Provided functions
get-q-hist"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; implemented here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-hist-extractor get-q-hist get-q-fn
  "get-q-hist Q-LEARNING-ALG.  Return a <numbered-set> consisting of the Q-functions corresponding to each history item of this learning algorithm.")

(defmethod get-policy ((alg <q-learning-algorithm>) ks)
  "Make a greedy policy using the Q-function (note that this isn't quite what it's supposed to mean in the case of, say, policy evaluation).  The policy will choose randomly at states it hasn't seen before."
  (make-instance 'policy:<greedy-policy> :q-function (get-q-fn alg ks)))

(defmethod get-value-fn (alg ks)
  "The q-function can also function as the value function."
  (get-q-fn alg ks))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; subclasses must implement
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric get-q-fn (alg ks)
  (:documentation "get-q-fn Q-LEARNING-ALG KNOWLEDGE-STATE.  Extract q-function estimate from knowledge state."))