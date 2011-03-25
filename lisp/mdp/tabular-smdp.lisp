(in-package mdp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass <tabular-smdp> (<smdp>)
  ((termination-vector 
    :type (array boolean 1)
    :writer set-term-vec
    :reader term-vec)
   (num-avail-actions-vector
    :type (array fixnum 1)
    :writer set-num-avail-actions-vec
    :reader num-avail-actions-vec)
   (original-smdp
    :type <smdp>
    :initarg :smdp
    :reader smdp)
   (smdp-trans
    :reader trans-vec
    :writer set-trans-vec
    :type (array * 1)))
  (:documentation "An SMDP where states and actions are represented as fixnums, and trans-dist, term-pred, and avail-actions are all represented with vectors.  Used as a more efficient internal representation by the dynamic programming algorithms.

Create from an SMDP using tabular-smdp
"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constructors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((m <tabular-smdp>) &rest args &key smdp)
  (let* ((states (state-set smdp))
	 (num-states (size states)))
    
    (labels 
	((renumber-outcome (outcome)
	   (make-outcome (item-number (outcome-state outcome) states)
	    (outcome-reward outcome) (outcome-duration outcome)))
	 
	 (make-state-action-trans-vec (s a)
	   (let ((dist (smdp-trans-dist smdp s a))
		 (l nil))
	     (do-sample-points (outcome prob dist l)
	       (when (> prob 0)
		 (push (cons (renumber-outcome outcome) prob) l)))))
	 
	 (make-state-trans-vec (s)
	   (if (terminal? smdp s)
	       #()
	     (mapset 'vector (lambda (a) (make-state-action-trans-vec s a))
		     (avail-actions smdp s)))))
	     
    
      ;; state set
      (set-state-set num-states m)
    
      ;; termination
      (set-term-vec (mapset 'vector (lambda (s) (terminal? smdp s)) states) m)
    
      ;; available actions
      ;; note that action a1 done at s1 and a2 done at s2 might end up having the same name
      ;; in the new scheme.  This doesn't affect the final answer.
      (set-num-avail-actions-vec 
       (mapset 'vector (lambda (s) (size (avail-actions smdp s))) states)
       m)
    
      ;; transition model
      (set-trans-vec (mapset 'vector #'make-state-trans-vec states) m))))

(defgeneric tabular-smdp (smdp)
  (:documentation "tabular-smdp SMDP.  Return a tabular smdp equivalent to SMDP.  There is an around method that first checks if there is a cached copy of it already stored, and otherwises calls the next method and caches the result.")
  (:method :around (smdp)
	   (or (cached-tabular-smdp smdp)
	       (setf (cached-tabular-smdp smdp)
		 (call-next-method))))
  (:method (smdp)
	   (make-instance '<tabular-smdp> :smdp smdp)))

    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smdp methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod terminal? ((m <tabular-smdp>) i)
  (aref (term-vec m) i))

(defmethod avail-actions ((m <tabular-smdp>) i)
  (aref (num-avail-actions-vec m) i))

(defmethod smdp-trans-dist ((m <tabular-smdp>) i j)
  (aref (aref (trans-vec m) i) j))
  


