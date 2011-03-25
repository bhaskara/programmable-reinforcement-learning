
(defpackage calisp-q-function
  (:documentation
   "Package for defining CALisp Q-functions 

Types
-----
<calisp-crl-q-function>
<threadwise-q-function>
<temporal-q-function>
<threadwise-crl-q-function>
<temporal-crl-q-function>

Defining feature templates
--------------------------
def-feature-template
def-decomposed-feature-template
make-feature-template
make-decomposed-feature-template
make-feature
omega
env-state
thread-states
threads-in-subtask
threads-at-label
make-calisp-feature
choosing-threads-at-label
bin-values

Other
-----
qr
qc
qe
")
   
  (:nicknames calisp-q-fn)
  (:use
   cl
   crlq
   dec-q-fn
   calisp-features
   utils)
  (:export
   <calisp-crl-q-function>
   <threadwise-q-function>
   <temporal-q-function>
   <threadwise-crl-q-function>
   <temporal-crl-q-function>
   def-feature-template
   def-decomposed-feature-template
   make-feature-template
   make-decomposed-feature-template
   make-feature
   omega 
   env-state
   thread-states
   threads-in-subtask
   threads-at-label
   make-calisp-feature
   choosing-threads-at-label
   bin-values
   qr
   qc
   qe))

(in-package calisp-q-function)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; coordinated q-functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <calisp-crl-q-function> (crlq:<crl-q-function> <calisp-q-function>)
  ((choice-fn :initform #'js-choices))
  (:documentation "Concurrent ALisp Q-function which is also a crl (coordinated relational linear) q-function (see <crl-q-function> class for details).  This allows maximization and exploration to be done efficiently.

Takes same initargs as crlq:<crl-q-function>, except :choice-fn must not be provided.
"))

(defclass <threadwise-q-function> (<decomposed-q-function> <calisp-q-function>)
  ((component-fn :initform #'threadwise-comps))
  (:documentation "A calisp q-function that is also a decomposed q-function, and for which there's one component per thread id at a state.  An abstract class because it doesn't actually define what the q-components are.  See also the <threadwise-crl-q-function> subclass."))

(defclass <temporal-q-function> (<decomposed-q-function> <calisp-q-function>)
  ((component-fn :initform #'temporal-comps))
  (:documentation "A calisp q-function that is also a decomposed q-function, and for which there's one component for each thread-temporal-component pair.  An abstract class because it doesn't actually define what the q-components are.  See also the <temporal-crl-q-function> subclass."))

(defclass <threadwise-crl-q-function> (<decomposed-crl-q-function> <threadwise-q-function> <calisp-crl-q-function>) ())
(defclass <temporal-crl-q-function> (<decomposed-crl-q-function> <temporal-q-function> <calisp-crl-q-function>) ())




(defun threadwise-comps (omega)
  (loop for ts being each hash-key in (js-thread-states omega) collecting ts))

(defparameter *q-comps* '(qr qc qe))

(defun temporal-comps (omega)
  (make-instance 'prod-set:<var-set> 
    :sets (list (threadwise-comps omega) *q-comps*)
    :element-type 'list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; feature templates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro make-feature-template ((&rest temp-args) &rest args)
  "Macro make-feature-temp (VAR-NAME) [DOC-STRING] &body BODY.

VAR-NAME (not evaluated) : a symbol
DOC-STRING : a string.  Defaults to the empty string.
BODY : sequence of forms surrounded by an implicit progn.

A macro for defining feature templates.  Expands to code that evaluates to a function that depends on a single argument called VAR-NAME.  The macro sets up various lexical bindings so that feature template descriptions that use make-feature-temp can be easier to read - see use-calisp-bindings for details."
  
  (condlet 
   (((stringp (first args)) (doc-string (first args)) (body (rest args)))
    ((not (stringp (first args))) (doc-string "") (body args)))
   
   (let ((omega (first temp-args)))
     (with-gensyms (t-ids fn)
       ;; expanded code begins here
       `(lambda ,temp-args
	  ,doc-string
	  (declare (ignorable ,@temp-args))
	  (use-calisp-bindings 
	   ,omega 
	   (flet
	       ((make-calisp-feature (,t-ids ,fn)
		  (make-feature ,t-ids ,fn choice-acc)))
	     ,@body)))))))

(defmacro make-decomposed-feature-template (&rest args)
  "Macro make-decomposed-feature-template (STATE-VAR-NAME ID-VAR-NAME) [DOC-STRING] &body BODY.

Works analogously to make-feature-template, except that it creates a function depending on *two* arguments - STATE-VAR-NAME and ID-VAR-NAME, representing the joint state and component ID.  The lexical bindings are the same as in make-feature-template."
  (assert (eql (length (first args)) 2))
  `(make-feature-template ,@args))


(defmacro def-feature-template (name &rest args)
  "Macro def-feature-temp NAME (JS-VAR) [DOC-STRING] &rest BODY
Makes NAME fbound to a function of one argument, JS-VAR, created using make-feature-template applied to the remaining arguments.  The optional DOC-STRING is empty if not supplied."
  `(setf (fdefinition ',name) (make-feature-template ,@args)))
	      

(defmacro def-decomposed-feature-template (name &rest args)
  "Macro def-decomposed-feature-template NAME (JS-VAR ID-VAR) [DOC-STRING] &rest BODY
Makes NAME fbound to a function of two arguments - JS-VAR and ID-VAR (unevaluated symbols) - created using make-decomposed-feature-template applied to the remaining arguments.  The optional DOC-STRING is empty if not supplied."
  `(setf (fdefinition ',name) (make-decomposed-feature-template ,@args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; print methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((q-fn <threadwise-q-function>) str)
  (if *print-readably*
      (progn
	(assert (eq (q-fn:featurizer q-fn) #'default-featurizer) ()
	  "Unable to print <approx-q-fn> readably since it does not use the default featurizer")
	(format str "#.(make-instance '~W :fn-approx "
		(type-of q-fn))
	(write (q-fn:fn-approx q-fn) :stream str)
	(format str ")"))
    (print-unreadable-object (q-fn str :type t :identity t))))