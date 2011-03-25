;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; defines the <env-q-function> subclass of <approx-q-function>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package q-function)


(defclass <env-q-function> (<approx-q-function>)
  ((env :type env-user:<env>
	:reader env
	:writer set-env
	:initarg :env)
   (env-name :type symbol
	     :reader env-name
	     :writer set-env-name
	     :initform nil
	     :initarg :env-name))
  (:documentation "An <env-q-function> is a type of <approx-q-function>.   Initargs
:env - the environment for which this is a Q-function. Its choices method asks the environment for the list of available actions (using avail-actions).
:env-name - a symbol which names a global variable that evaluates to the given environment.  This argument only needs to be supplied if you want to ever print the q-function out readably.  When reading it back in, the name still has to refer to the environment.

and the initargs from <approx-q-function>
"))


(defmethod choices ((q <env-q-function>) s)
  (env-user:avail-actions (env q) s))


(defmethod clone ((q <env-q-function>))
  "Just uses the parent's clone method and then does a change-class.  Change-class is risky but should be ok here because we're changing to a subclass of the original class."
  (check-exact-class q '<env-q-function>)
  (let ((new-q (call-next-method)))
    (change-class new-q '<env-q-function>)
    (set-env (env q) new-q)
    (set-env-name (env-name q) new-q)
    new-q))


(defmethod print-object ((q-fn <env-q-function>) str)
  (if *print-readably*
      (let ((name (env-name q-fn)))
	(assert name nil "Can't print <env-q-function> readably because the env-name field is not set.")
	(check-exact-class q-fn '<env-q-function>)
	(format str "#.(let ((q ")
	(q-fn:print-approx-q-function-readably q-fn str)
	(format str ")) (q-fn::set-env ~W q) (q-fn::set-env-name '~:*~W q) q)"  name))
    (call-next-method)))
