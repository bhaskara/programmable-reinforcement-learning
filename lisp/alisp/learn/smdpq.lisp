(defpackage alisp-smdpq
  (:nicknames asq)
  (:documentation "Package alisp-smdpq.  The smdp q-learning algorithm, applied to alisp. 

Types
<smdpq>

See also alisp-user package for other operations on <alisp-learning-algorithm> objects
")
  (:export
   <smdpq>
   make-smdpq-alg)
  (:use common-lisp
	utils
	policy
	alisp-obs))


(in-package alisp-smdpq)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; class def
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass <smdpq> (<alisp-learning-algorithm> <q-learning-algorithm>)
  ((prev-omega :accessor prev-omega)
   (prev-u :accessor prev-u)
   (total-reward :accessor rew)
   (q-function :reader q-fn :type q-fn:<q-function> :initarg :q-fn :writer set-q-fn)
   (init-q-function :reader init-q-fn :type q-fn:<q-function> :writer set-init-q-fn)
   (discount :reader disc :initarg :disc :initform 1.0)
   (total-discount :accessor total-disc)
   (learning-rate :reader lrate :initarg :lrate))
    
  (:documentation "Class <smdpq>.  Subclass of <alisp-learning-algorithm> that uses SMDP Q-learning.

Initargs
:disc discount.  1.0 by default.
:lrate learning rate
:q-fn q-function
"))

(defmethod initialize-instance :after ((alg <smdpq>) &rest args)
  (declare (ignore args))
  (set-init-q-fn (clone (q-fn alg)) alg))

(defun make-smdpq-alg (&key (lrate .02) (discount 1.0)
				     (q (make-instance 'alisp-obs:<alisp-approx-q-function>
					       :fn-approx (make-instance 'fn-approx:<tabular-fn-approx>)))
				     (debug-stream nil) (hist-out-dir nil))
  "make-smdpq-alg &key (LRATE .01) (DISCOUNT 1.0) (Q-FN) (DEBUG-STREAM nil) (HIST-OUT-DIR nil)
Make an instance of smdp q-learning.  If Q-FN not provided, use tabular rep."
  (make-instance '<smdpq> :lrate lrate :disc discount :q-fn q :debug-str debug-stream :hist-out-dir hist-out-dir))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; methods from <alisp-learning-algorithm>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod reset ((alg <smdpq>))
  (set-q-fn (clone (init-q-fn alg)) alg))


(defmethod inform-start-episode ((alg <smdpq>) s)
  (declare (ignore s))
  (setf (prev-omega alg) nil
	(prev-u alg) nil
	(rew alg) 0))
  
  
(defun smdpq-finish-episode (alg)
  ;; do a final backup if possible
  (let ((prev-omega (prev-omega alg))
	(prev-u (prev-u alg)))
    (when prev-omega
      (q-fn:update (q-fn alg) prev-omega prev-u (rew alg)
		   (lrate:get-rate (lrate alg) (cons prev-omega prev-u)))
      (debug-msg alg "~&Episode terminated.  Backup to state ~a and action ~a with old value ~a towards new value ~a."
		 prev-omega prev-u (q-fn:evaluate (q-fn alg) prev-omega prev-u) (rew alg))
      (setf (prev-omega alg) nil
	    (prev-u alg) nil))))
   
(defmethod inform-env-step ((alg <smdpq>) act rew to term)
  (declare (ignore to act))
  ;; accumulate reward
  (incf (rew alg) (* (total-disc alg) rew))
  (_f * (total-disc alg) (disc alg))
  (debug-msg alg "~&Accumulated reward for SMDPQ is ~a" (rew alg))
  (when term (smdpq-finish-episode alg)))


(defmethod inform-alisp-step ((alg <smdpq>) omega u)
  
  (assert (not (exit-state? omega)))
  
  (unless (action-state? omega)

    (with-slots (prev-omega prev-u q-function) alg
   
      ;; when possible, do a backup
      (when prev-omega
	(handler-case
	    (let ((new-val (+ (rew alg)
			      (* (total-disc alg)
				 (q-fn:value q-function omega)))))
	      (debug-msg alg "~&Backup to state ~a and choice ~a with old value ~a towards new value ~a"
			 prev-omega prev-u (q-fn:evaluate q-function prev-omega prev-u) new-val)
	      (q-fn:update q-function prev-omega prev-u new-val
			   (lrate:get-rate (lrate alg) (cons prev-omega prev-u))))
	  (q-fn:unknown-state-action ()
	    (debug-msg alg "~&Q-value unknown for state ~a" omega)
	    ))))
    
    (setf (prev-omega alg) omega
	  (prev-u alg) u
	  (rew alg) 0
	  (total-disc alg) 1.0)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; knowledge state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod knowledge-state ((alg <smdpq>) &optional (fresh t))
  (let ((q (q-fn alg)))
    (if fresh (clone q) q)))

(defmethod get-q-fn ((alg <smdpq>) ks)
  ks)




