;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; concurrent-alisp/calisp.lisp - defines and documents the various packages in the calisp code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage calisp
  (:documentation "Package calisp.  The internal namespace for all code related to Concurrent ALisp.  For more information see help for
calisp-user
calisp-prog
calisp-obs")

  #+short-nicknames (:nicknames c)

  (:use common-lisp
	utils
	mp
	cv
	threads
	set
	prob
	rl-obs)
  (:import-from
   q-fn
   <q-function>
   unknown-state-action))



(defpackage calisp-user
  (:documentation "Package calisp-user.  Package used when running concurrent alisp.

Exported symbols
================

Running concurrent alisp programs
---------------------------------
io-interface
learn
on-policy-learning
evaluate

Creating partial program objects
--------------------------------
<calisp-program>
all
highest-level-first
highest-level-single



Operations on learning algorithms
---------------------------------
hist
get-policy-hist
get-q-hist
reset
set-hist-collect
debug-str
set-debug-str

Other
-------
*hist*
no-choice
*io-interface-debug-threads*
*calisp-random-policy*

Related packages
================

Observers
---------
calisp-io-int-observer



")
  (:use common-lisp
	package-utils))

(in-package calisp-user)

(export-from 'calisp
	     "io-interface"
	     "learn"
	     "on-policy-learning"
	     "evaluate"
	     "<calisp-program>"
	     "all"
	     "highest-level-first"
	     "highest-level-single"
	     "*hist*"
	     "no-choice"
	     "*io-interface-debug-threads*"
	     "*calisp-random-policy*"
	     )

(export-from 'rl
	     "hist"
	     "get-policy-hist"
	     "reset"
	     "set-hist-collect"
	     "get-q-hist"
	     "debug-str"
	     "set-debug-str")



(in-package cl-user)

(defpackage calisp-obs
  (:documentation "Package calisp-obs.  Package used when creating new observers.  A <calisp-observer> is any object that observes the execution of an concurrent alisp program.  This can include learning algorithms (see subdirectory learn/), and also objects that maintain logs or statistics of execution (see subdirectory obs/). 

All observers must inherit from <calisp-observer>.  They may also inherit from <calisp-learning-algorithm>, <q-learning-algorithm>, or <policy-learning-algorithm>.
  
Subclasses may implement any subset of inform- methods listed below.

Note : many of the inform- methods pass some representation of the current environment or joint state to the observer.  This object should never be modified, since the environment or concurrent ALisp system might hold a reference to it.  Also, observers should not assume that this object will stay unchanged - if they need to store it for future use, they should clone it.

Types
-----
<calisp-observer>
<calisp-learning-algorithm>
<on-policy-learning-algorithm>
<q-learning-algorithm>
<policy-learning-algorithm>
<calisp-q-function>
<calisp-approx-q-function>

Messages received by an observer
--------------------------------

inform-start-execution
inform-finish-execution
inform-start-episode
inform-env-step
inform-calisp-step
inform-arrive-choice-state
inform-end-choice-block
inform-part-prog-terminated
inform-spawn-thread
inform-reassign
inform-die-thread

Messages received only by debugging observers
---------------------------------------------

inform-wait-action
inform-wait-choice
inform-wait-effectors
inform-wakeup
inform-main-thread-wait
inform-main-thread-wakeup


Other methods for learning algorithms
-------------------------------------
knowledge-state
get-q-fn
get-policy
get-value-fn
reset

Operations for use by learning algorithms
-----------------------------------------
current-env-step
current-episode-step
debug-str

Accessing state
---------------
no-choice
js-pc
js-choices
js-thread-states
js-thread-label
ts-status
choosing
with-choice
spawn
choose
call
action
ts-type

Macros
------
defmessage

")

  (:use common-lisp
	package-utils))

(in-package calisp-obs)

(export-from 'calisp
	     "<calisp-observer>"
	     "<calisp-learning-algorithm>"
	     "<calisp-q-function>"
	     "<calisp-approx-q-function>"
	     "inform-calisp-step"
	     "inform-arrive-choice-state"
	     "inform-end-choice-block"
	     "inform-part-prog-terminated"
	     "no-choice"
	     "js-pc"
	     "js-choices"
	     "js-thread-states"
	     "ts-status"
	     "ts-type"
	     "js-thread-label" 
	     "choosing"
	     "with-choice"
	     "spawn"
	     "choose"
	     "call"
	     "action"
	     "inform-spawn-thread"
	     "inform-reassign"
	     "inform-die-thread"
	     "inform-wait-action"
	     "inform-wait-choice"
	     "inform-wait-effectors"
	     "inform-wakeup"
	     "inform-main-thread-wait"
	     "inform-main-thread-wakeup")

(export-from 'rl
	     "<on-policy-learning-algorithm>"
	     "<q-learning-algorithm>"
	     "<policy-learning-algorithm>"
	     "defmessage"
	     "inform-start-episode"
	     "inform-env-step"
	     "inform-start-execution"
	     "inform-finish-execution"
	     "reset"
	     "current-env-step"
	     "current-episode-step"
	     "debug-str"
	     "get-q-fn"
	     "knowledge-state"
	     "get-policy"
	     "get-value-fn"
	     "defmessage"
	     "debug-msg")


(in-package cl-user)

(defpackage calisp-prog
  (:documentation "Package calisp-prog.  Package used by concurrent Alisp partial programs.
Types :
- <calisp-program>

Operations called from partial programs :
- action
- with-choice
- choose
- dummy-choice
- call
- choose-arg
- spawn
- get-new-thread-id
- reassign
- wait-for-effectors
- my-effectors
- env-state
- combined-state
- env-has-terminated
- def-env-accessor
- mem
- gmem

Operations implemented by partial programs
- start
- assign-effectors

Choosing threads functions
- highest-level-first
- highest-level-single
- all
- ordered-choosing-threads-function


Other
- root-thread
")


  (:use common-lisp
	package-utils))

(in-package calisp-prog)

(export-from 'calisp
	     "<calisp-program>"
	     "action"
	     "choose"
	     "dummy-choice"
	     "with-choice"
	     "call"
	     "choose-arg"
	     "env-state"
	     "combined-state"
	     "env-has-terminated"
	     "spawn"
	     "get-new-thread-id"
	     "reassign"
	     "wait-for-effectors"
	     "my-effectors"
	     "def-env-accessor"
	     "mem"
	     "gmem"
	     "start"
	     "assign-effectors"
	     "highest-level-first"
	     "highest-level-single"
	     "all"
	     "ordered-choosing-threads-function"
	     "root-thread"
	     )

(in-package cl-user)

(defpackage calisp-features
  (:documentation "Package used when defining features for value/q-functions.

Types
-----
<calisp-q-function>

Accessing state
---------------
use-calisp-bindings
js-choices
js-choosing-thread-ids
js-env-state
js-thread-states
js-thread-label
thread-states
env-state
at-label
choosing
threads-in-subtask
threads-at-label
choosing-threads-at-label
choice-acc
ts-stack
stack-var-val
ts-label
some-frame-at-label
some-frame-has-name
js-thread-numbers
make-calisp-feature

Other
-----
root-thread

")
  (:use
   cl
   package-utils))

(in-package calisp-features)

(export-from 'calisp
	     "<calisp-q-function>"
	     "use-calisp-bindings"
	     "js-choices"
	     "js-choosing-thread-ids"
	     "js-env-state"
	     "js-thread-states"
	     "thread-states"
	     "env-state"
	     "js-thread-label"
	     "at-label"
	     "choosing"
	     "threads-in-subtask"
	     "threads-at-label"
	     "choosing-threads-at-label"
	     "choice-acc"
	     "ts-stack"
	     "stack-var-val"
	     "ts-label"
	     "some-frame-at-label"
	     "some-frame-has-name"
	     "js-thread-numbers"
	     "make-calisp-feature"
	     "root-thread")

  

(in-package calisp)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dynamic variables internal to concurrent alisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar *crlm* nil "When executing a concurrent alisp program, holds the corresponding CRLM")
(defvar *containing-subroutine-name* nil "Holds the name of the most (dynamically) recently entered concurrent ALisp subroutine that has not yet exited.  Not used at the moment.")

