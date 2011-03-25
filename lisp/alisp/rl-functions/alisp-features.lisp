;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; alisp-features.lisp
;; code that allows definition of features for function approximation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package alisp)


(defmacro use-alisp-state-bindings (omega &rest body)
  "use-alisp-state-bindings JS-VAR-NAME &rest BODY

JS-VAR-NAME (unevaluated) - a symbol
BODY (unevaluated) - list of forms

Evaluate BODY with various lexical bindings in effect.  JS-VAR-NAME is assumed bound in the surrounding context to an alisp joint state.  

Variable bindings
-----------------
ENV-STATE - env state
LABEL - label of current choice point

Function bindings
-----------------
stack-var-val NAME &optional (INCLUDE-NEXT-FRAME nil).  Returns value of a variable in stack.
stack-contains-frame NAME.  Does the stack contain a frame named NAME?"

  (with-gensyms (args name)
    `(flet ((stack-var-val (&rest ,args)
	      (apply #'stack-var-val ,omega ,args))
	    (stack-contains-frame (,name)
	      (stack-contains-frame ,omega ,name)))
    
       (let ((env-state (js-env-state ,omega))
	     (label (program-counter-label (js-pc ,omega))))
	 (declare (ignorable env-state label))
	 ,@body))))


(defmacro make-feature ((omega u) &rest args)
  "make-feature (STATE-VAR-NAME CHOICE-VAR-NAME) [DOC-STRING] &rest BODY

STATE-VAR-NAME (unevaluated) - a symbol
CHOICE-VAR-NAME (unevaluated) - a symbol
DOC-STRING (unevaluated) - a string.  Defaults to empty string.
BODY (unevaluated) - list of forms.

Return a function of two arguments named STATE-VAR-NAME and CHOICE-VAR-NAME that evaluates BODY with a set of additional lexical bindings in effect (see use-alisp-state-bindings)."

  (condlet
   (((stringp (first args)) (doc-string (first args)) (body (rest args)))
    (t (doc-string "") (body args)))
   
   ;; expanded code begins here
   `(lambda (,omega ,u)
      ,doc-string
      
      (declare (ignorable ,u))
      (use-alisp-state-bindings
       ,omega
       ,@body))
   ))

(defmacro def-feature (name (omega u) &rest args)
  "def-feature NAME (STATE-VAR-NAME CHOICE-VAR-NAME) [DOC-STRING] &rest BODY

Like defun, except certain lexical bindings are available within BODY.  See make-feature for details."
  `(setf (fdefinition ',name) (make-feature (,omega ,u) ,@args)))


(defmacro make-3partq-featurizer (options &rest defs)
  "Macro make-3partq-featurizer OPTIONS &rest CLAUSES

OPTIONS (not evaluated) - list of options
CLAUSES - list of clauses (not evaluated)

For now, OPTIONS must be nil.

Each CLAUSE is a list of the form (CP-SPEC &rest Q-COMP-SPECS).
CP-SPEC (unevaluated) is an symbol.
Each element of Q-COMP-SPECS is a list (unevaluated) of the form (COMP-NAME &rest FEATURES)
COMP-NAME (not evaluated) is one of :qr-depends, :qc-depends, or :qe-depends
There can be at most one element of Q-COMP-SPECS having a given COMP-NAME
Each element of FEATURES is either a symbol, which designates a function with that name, or a nonsymbol, which designates the function it evaluates to.

The macro overall expands to code that returns a list containing three elements, namely the featurizers for Qr, Qc, and Qe.  Each featurizer is a function that takes in a joint state and choice.  The function first looks at the label of the choice point and selects a CLAUSE whose CP-SPEC is equal to this label.  It then selects the appropriate Q-COMP-SPEC (the one having the right COMP-NAME).  Finally, it applies each of the features in this Q-COMP-SPEC to the state and choice, and collects the values into a vector.

In addition, make-3partq-featurizer binds certain function symbols for use as features
- choice just returns the choice"
  
  (declare (ignore options))
  `(flet
       ((choice (omega u) (declare (ignore omega)) u))
       
     (list
      ,@(mapcar
	 
	 ;; function that returns code that expands to featurizer description
	 ;; of one of the components
	 (lambda (comp-name)
	   `(make-feature 
	     (omega u)
	     (ecase label ,@(mapcar (lambda (def) (make-def-case def comp-name)) defs))))
	 
	 ;; the components
	 '(:qr-depends :qc-depends :qe-depends)))))



(defun make-def-case (def comp-name)
  "make-def-case DEF COMP-NAME
Helper function for make-3partq-featurizer."
  
  (let ((clause (assoc comp-name (rest def)))
	(v (gensym))
	(name (first def)))
    `(,name
      (let ((,v (make-array ,(max (length clause) 1))))
	(setf (aref ,v 0) ',name)
	,@(loop
	      for i from 1
	      for c in (rest clause)
	      collect `(setf (aref ,v ,i) (,c omega u)))
	,v))))