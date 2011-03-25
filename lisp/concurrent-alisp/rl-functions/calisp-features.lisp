(in-package calisp)

(defmacro use-calisp-bindings (omega &rest body)
  "use-calisp-bindings JOINT-STATE-NAME &rest BODY

JOINT-STATE-NAME (not evaluated) : a symbol
BODY (not evaluated) : a list of forms surrounded by an implicit progn

Evaluate BODY with the following lexical bindings in effect.
	   
Variable bindings
-----------------
JOINT-STATE-NAME - joint state
ENV-STATE - environment state
THREAD-STATES - thread states
CHOICE-ACC - object of type inst-var-accessors that describes how to access components of the joint choice.  See the inst-vars package for more details.

Macro bindings
--------------
threads-in-subtask NAME returns a list of threads whose stack contains a frame named NAME.  The argument NAME is an unevaluated symbol.
threads-at-location LABEL returns a list of threads which are at a location with label LABEL.  LABEL is an unevaluated symbol."
  
  (with-gensyms (task-name label)
    `(flet ((get-threads-in-subtask (,task-name)
	      (threads-in-subtask ,omega ,task-name))
	    (get-threads-at-label (,label)
	      (threads-at-label ,omega ,label))
	    (get-choosing-threads-at-label (,label)
	      (choosing-threads-at-label ,omega ,label))
	    )
	     
       (macrolet
	   ((threads-in-subtask (,task-name)
	      `(get-threads-in-subtask ',,task-name))
	    (threads-at-label (,label)
	      `(get-threads-at-label ',,label))
	    (choosing-threads-at-label (,label)
	      `(get-choosing-threads-at-label ',,label)))
	    
	 (let ((env-state (js-env-state ,omega))
	       (thread-states (js-thread-states ,omega))
	       (choice-acc (prod-set:inst-acc (js-choices ,omega))))

	   (declare (ignorable env-state thread-states ,omega))
	   ,@body)))))

 
		     
	
       

