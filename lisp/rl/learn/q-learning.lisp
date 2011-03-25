(defpackage q-learning
  (:documentation "Defines the <q-learning> learning algorithm.

Exports
-------
make-q-learning-alg
<q-learning>")
  (:import-from 
   rl
   make-q-learning-alg)
  (:export 
   make-q-learning-alg
   <q-learning>)
  (:use 
   rl-obs
   cl
   utils))

(in-package q-learning)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Class def
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass <q-learning> ( <q-learning-algorithm>)
  ((discount :type float
	     :initarg :discount
	     :initform 1.0
	     :reader discount)
   (learning-rate :type lrate:<learning-rate>
		  :initarg :lrate
		  :initform .01
		  :reader lrate)
   (q-function :type q-fn:<q-function>
	       :reader q-fn
	       :initarg :q-fn
	       :initform nil
	       :writer set-q-fn)
   (init-q-function :type q-fn:<q-function>
		    :reader init-q-fn
		    :writer set-init-q-fn)
   (prev-s :accessor prev-s)
   (prev-a :accessor prev-a))
  (:documentation "The <q-learning> class.  Inherits from <learning-algorithm>.  Create using make-instance. Initargs

:lrate - <learning-rate>
:q-fn - <q-function> (defaults to a tabular q-fn - see the env initarg below)
:env - if q-fn initarg not provided, must provide this instead.
:discount (optional, default 1.0)
as well as the initargs from <learning-algorithm>


When asked for its knowledge state, a <q-learning> object returns the currently estimated Q-function.  Its convert-to-policy method turns a Q-function into the corresponding greedy policy.
"))


(defmethod initialize-instance :after ((alg <q-learning>) &rest args &key env)
  (declare (ignore args))
  (unless (q-fn alg)
    (set-q-fn (make-instance 'q-fn:<env-q-function> :env env) alg))
  (set-init-q-fn (clone (q-fn alg)) alg))



; (defun make-q-learning-alg (env &key (lrate .01) 
; 				 (q-fn (q-fn:make-tabular-env-q-function env))
; 				     (discount 1.0)
; 				     (debug-str t))
;   "make-q-learning-alg ENV &key (LRATE .01) (Q-FN (a tabular q-function for env)) (discount 1.0) (DEBUG-STR t)"
;   (make-instance '<q-learning> :lrate lrate :q-fn q-fn :discount discount :debug-str debug-str))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operations from <rl-observer>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod inform-start-episode ((alg <q-learning>) s)
  (setf (prev-s alg) s))

(defmethod inform-env-step ((alg <q-learning>) a r s2 term)
  (let* ((q-fn (q-fn alg))
	 (prev-s (prev-s alg))
	 (eta (lrate:get-rate (lrate alg) (cons prev-s a))))
    (if term
	(progn
	  (q-fn:update q-fn prev-s a r eta)
	  (setf (prev-s alg) nil))
      (progn
	(q-fn:update q-fn prev-s a
		     (+ r 
			(* (discount alg) 
			   (handler-case
			       (q-fn:value q-fn s2)
			     (q-fn:unknown-state-action () 0))))
		     eta)
	(setf (prev-s alg) s2)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operations from <q-learning-algorithm>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod reset ((alg <q-learning>))
  (set-q-fn (clone (init-q-fn alg)) alg)
  (lrate:reset-counts (lrate alg)))

(defmethod knowledge-state ((alg <q-learning>) &optional (fresh t))
  "The state of knowledge is just the Q-function."
  (let ((q (q-fn alg)))
    (if fresh (clone q) q)))

(defmethod get-q-fn ((alg <q-learning>) ks)
  ks)

		  



(in-package cl-user)

   