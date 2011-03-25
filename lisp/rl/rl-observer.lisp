;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rl/rl-observer.lisp
;; Defines the <rl-observer> abstract class and some generic functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package rl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; class def
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <rl-observer> ()
  ()
  (:documentation "Class <rl-observer>.    An rl-observer is any object that observes execution during reinforcement learning.  This can include learning algorithms (see subdirectory learn/), and also objects that maintain logs or statistics of execution (see subdirectory obs/). 
  
Subclasses may implement any subset of the following methods.  If a subclass doesn't implement a method, that just means the corresponding event is ignored by that type of observer.
- inform-start-episode
- inform-env-step
- inform-start-execution
- inform-finish-execution
"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; definition macro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro defmessage (name lambda-list &optional (doc-string ""))
  "defmessage NAME LAMBDA-LIST DOC-STRING.  Used to define messages to a rl-observer (as generic functions).  As an example, (defmessage inform-reset (s) doc-string) expands to code that creates a generic function inform-reset with two arguments - observer and s, documentation field doc-string, and a single method defined for the case when observer is of type <observer> that does nothing.  See examples in this file."
  (with-gensyms (observer)
    `(defgeneric ,name ,(cons observer lambda-list)
		 (:documentation ,doc-string)
       (:method ,(cons `(,observer <rl-observer>) lambda-list) (declare (ignore ,@lambda-list)) ()))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; kinds of messages that can be sent to an observer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmessage inform-start-episode (s)
  "inform-start-episode OBSERVER NEW-STATE.  Inform observer that environment has been reset to state NEW-STATE.")

(defmessage inform-env-step (act rew to term)
  "inform-env-step OBSERVER ACTION REWARD NEXT-STATE TERM.  Inform observer of that ACTION was done in the environment, resulting in new state NEXT-STATE, REWARD units of reward.  TERM is true iff the environment has now terminated. (Note that the observer will always know the previous state - because of a previous inform-start-episode or inform-env-step message.")

(defmessage inform-finish-execution ()
  "inform-finish-execution OBSERVER.  Inform observer that the current run is about to finish (i.e. we may not have reached the end of an episode, but we're going to stop anyway).")

(defmessage inform-start-execution ()
  "inform-start-execution OBSERVER.  Inform observer that we've just started a new run.")

  


