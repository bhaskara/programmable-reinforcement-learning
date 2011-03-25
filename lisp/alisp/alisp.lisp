;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; alisp.lisp - defines and documents the various packages in the alisp code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage alisp
  (:documentation "Package alisp.  The internal namespace for all code related to ALisp.  For more information see help for
alisp-user
alisp-prog
alisp-features
alisp-obs")

  (:use common-lisp
	utils
	prob
	rl-obs)
  (:import-from
   q-fn
   <q-function>
   unknown-state-action
   )
  (:import-from
   pol
   choose-to-abort)
  )



(defpackage alisp-user
  (:documentation "Package alisp-user.  Package used when running alisp.

Exported symbols
================

Running alisp
-------------
- io-interface
- learn
- evaluate

Operations on learning algorithms
---------------------------------
hist
get-policy-hist
get-q-hist
reset
set-hist-collect

Parameters
----------
*clone-quickly*
*canonicalize-quickly*

Q-functions
-----------
<alisp-approx-q-function>

Other
-------
*hist*
no-choice


Related packages
================

Observers
---------
alisp-io-int-observer
alisp-smdpq
alisp-gs
alisp-hordq

")
  (:use common-lisp
	package-utils))

(in-package alisp-user)

(export-from 'alisp
	     "io-interface"
	     "learn"
	     "evaluate"
	     "*hist*"
	     "no-choice"
	     "*clone-quickly*"
	     "*canonicalize-quickly*"
	     "<alisp-approx-q-function>"
	     )

(export-from 'rl
	     "hist"
	     "get-policy-hist"
	     "reset"
	     "set-hist-collect"
	     "get-q-hist")



(in-package cl-user)

(defpackage alisp-obs
  (:documentation "Package alisp-obs.  Package used when creating new observers.  An alisp-observer is any object that observes the execution of an alisp program.  This can include learning algorithms (see subdirectory learn/), and also objects that maintain logs or statistics of execution (see subdirectory obs/). 

All alisp observers must inherit from <alisp-observer>.  They may also inherit from <alisp-learning-algorithm>, <q-learning-algorithm>, <policy-learning-algorithm>, <value-learning-algorithm>, or <alisp-model-learning-algorithm>.
  
Subclasses may implement any subset of inform- methods listed below.

Note : many of the inform- methods pass some representation of the current environment or joint state to the observer.  This object should never be modified, since the environment or ALisp system might hold a reference to it.  Also, observers should not assume that this object will stay unchanged - if they need to store it for future use, they should clone it.

Types
-----
<alisp-observer>
<alisp-learning-algorithm>
<q-learning-algorithm>
<policy-learning-algorithm>
<value-learning-algorithm>
<alisp-model-learning-algorithm>
<alisp-approx-q-function>
<holy-q-function>

Constructors
------------
make-exit-dist
make-holy-q-fn


Messages received by an algorithm
---------------------------------

inform-start-execution
inform-finish-execution
inform-start-episode
inform-env-step
inform-alisp-step
inform-part-prog-terminated
inform-end-choice-block


Other methods for learning algorithms
-------------------------------------
knowledge-state
get-q-fn
get-policy
get-value-fn
reset

Accessing state
---------------
js-choices
no-choice
action-state?
exit-state?

Macros
------
defmessage
debug-msg


")

  (:use common-lisp
	package-utils))

(in-package alisp-obs)

(export-from 'alisp
	     "<alisp-observer>"
	     "<alisp-learning-algorithm>"
	     "<alisp-approx-q-function>"
	     "<alisp-model-learning-algorithm>"
	     "<holy-q-fn>"
	     "make-exit-dist"
	     "make-holy-q-fn"
	     "inform-alisp-step"
	     "inform-part-prog-terminated"
	     "inform-end-choice-block"
	     "js-choices"
	     "js-type"
	     "no-choice"
	     "action-state?"
	     "exit-state?"
	     )

(export-from 'rl
	     "<q-learning-algorithm>"
	     "<policy-learning-algorithm>"
	     "<value-learning-algorithm>"
	     "defmessage"
	     "inform-start-episode"
	     "inform-env-step"
	     "inform-start-execution"
	     "inform-finish-execution"
	     "reset"
	     "get-q-fn"
	     "knowledge-state"
	     "get-policy"
	     "get-value-fn"
	     "defmessage"
	     "debug-msg")


(in-package cl-user)

(defpackage alisp-features
  (:documentation "Package alisp-features.  Package used when writing features, e.g. for Q-functions.


Referring to parts of joint state
---------------------------------
js-label
js-env-state
js-num-choices

Features
--------
make-feature
def-feature
make-3partq-featurizer
choice
omega
u
env-state
stack-var-val
stack-contains-frame")



  (:use
   common-lisp
   package-utils))

(in-package alisp-features)

(export-from 
 'alisp
 "js-label"
 "js-env-state"
 "js-num-choices"
 "make-feature"
 "def-feature"
 "use-alisp-state-bindings"
 "make-3partq-featurizer"
 "choice"
 "omega"
 "env-state"
 "stack-var-val"
 "stack-contains-frame"
 )

(in-package cl-user)
   


(defpackage alisp-prog
  (:documentation "Package alisp-prog.  Package used by alisp partial programs.
Types :
- <alisp-program>

Operations called from partial programs :
- action
- with-choice
- choose
- call
- env-state
- env-has-terminated
- def-env-accessor
- mem
- gmem

Operations implemented by partial programs
- start
")


  (:use common-lisp
	package-utils))

(in-package alisp-prog)

(export-from 'alisp
	     "<alisp-program>"
	     "action"
	     "choose"
	     "with-choice"
	     "call"
	     "env-state"
	     "env-has-terminated"
	     "def-env-accessor"
	     "mem"
	     "gmem"
	     "start"
	     )





(in-package alisp)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dynamic variables internal to alisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar *rlm* nil "When executing an alisp program, holds the corresponding rlm")
(defvar *containing-subroutine-name* nil "Holds the name of the most (dynamically) recently entered ALisp subroutine that has not yet exited.")

