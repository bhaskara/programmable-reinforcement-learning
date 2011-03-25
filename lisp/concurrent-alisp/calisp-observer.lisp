;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; concurrent-alisp/calisp-observer.lisp
;; Defines the <calisp-observer> abstract class and some generic functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package calisp)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; class defs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <calisp-observer> (<rl-observer>)
  ()
  (:documentation "Class <calisp-observer>.  Subclass of rl:<rl-observer>.  See calisp-observer package for documentation."))

(defclass <calisp-debugging-observer> (<calisp-observer>)
  ()
  (:documentation "A kind of observer that can receive extra debugging messages."))


(defclass <calisp-learning-algorithm> (<calisp-observer> <learning-algorithm>)
  ()
  (:documentation "A <calisp-learning-algorithm> is just a particular type of <calisp-observer> that learns a policy (or Q-function or model) from experience in the environment."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; kinds of messages that can be sent to an calisp-observer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmessage inform-arrive-choice-state (omega)
  "Sent when a choice state has been reached but before a decision has been made.")

(defmessage inform-calisp-step (omega choice)
  "Sent when a joint step, consisting of call, choose, with-choice, and spawn statements has taken place.  See the reference for more details.")

(defmessage inform-part-prog-terminated ()
  "inform-part-prog-terminated OBSERVER.  Inform observer that the partial program has terminated, either because the environment reached a terminal state, or the last step of learning was reached.")

(defmessage inform-spawn-thread (id current-thread effectors)
  "inform-spawn-thread OBS ID THREAD-ID EFFECTORS.  Sent when a new thread with id ID has been created, with the given effector set.")

(defmessage inform-die-thread (current-thread)
  "inform-die-thread OBS THREAD-ID.  Sent when thread is about to die, i.e., no more choice blocks will be entered, and no more actions will be done.  If the environment terminates, therefore ending the episode, this message will be correctly sent to all threads.  It will not, however, be sent if the partial program is artificially terminated by the CRLM due to the last iteration of the main loop being reached.")

(defmessage inform-end-choice-block (current-thread omega)
  "inform-end-choice-block OBSERVER CURRENT-THREAD OMEGA.  Inform observer that the most recent choice block of CURRENT-THREAD has been exited.")

(defmessage inform-reassign (effectors src dest)
  "inform-reassign OBSERVER EFFECTORS SRC-THREAD-ID DEST-THREAD-ID.")




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; messages for debugging observers only
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmessage inform-wait-action (state id label) "TODO Document")
(defmessage inform-wakeup (state id) "TODO document")
(defmessage inform-main-thread-wait (state) "TODO document")
(defmessage inform-main-thread-wakeup (state) "TODO document")
(defmessage inform-wait-choice (state id label) "TODO document")
(defmessage inform-wait-effectors (state id) "TODO document")
  
				   



  


