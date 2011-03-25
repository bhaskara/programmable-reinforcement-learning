;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rl-functions/q-function/tabular-q-function.lisp
;; defines the <tabular-q-function> class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package q-function)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; class def
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <tabular-q-function> (<q-function>)
  ((state-set :initarg :state-set
	      :reader state-set
	      :type [numbered-set])
   (action-sets :initarg :action-sets
		:reader action-sets
		:type (simple-array [numbered-set] 1))
   (table :initarg :table
	  :reader table
	  :type (simple-array float 1)))
  (:documentation "A <tabular-q-function> has initargs
:state-set
:action-sets
:table

and represents a q-function by an array."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constructor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-tabular-q-function (table &optional (state-set nil) (action-sets nil))
  "make-tabular-q-function TABLE &optional STATE-SET ACTION-SETS.  TABLE is an array with one entry per state, in which each entry is an array (one entry per action available at that state).  If STATE-SET and ACTION-SETS are not provided, then sets of the form {0,1,...,n} (of the appropriate sizes) are used.  ACTION-SETS be an array of action sets for each state"
  (unless state-set
    (setf state-set (length table)))
  (unless action-sets
    (setf action-sets (map 'vector #'length table)))
  (make-instance '<tabular-q-function>
    :state-set state-set :action-sets action-sets :table table))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; implemented methods from <q-function>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod best-choice ((q <tabular-q-function>) s)
  (handler-case
      (let ((i (item-number s (state-set q))))
	(multiple-value-bind (j val)
	    (argmax (aref (table q) i))
	  (values (item j (aref (action-sets q) i)) val)))
    (item-not-in-set ()
      (error 'unknown-state-action :state s :action nil :q-fn q))))

(defmethod evaluate ((q <tabular-q-function>) s a)
  (handler-case
      (let ((i (item-number s (state-set q))))
	(aref (aref (table q) i)
	      (item-number a (aref (action-sets q) i))))
    (item-not-in-set ()
      (error 'unknown-state-action :state s :action a :q-fn q))))

(defmethod choices ((q <tabular-q-function>) s)
  (handler-case
      (aref (action-sets q) (item-number s (state-set q)))
    (item-not-in-set ()
      (error 'unknown-state :state s :q-fn q))))


(defmethod boltzmann-dist ((q <tabular-q-function>) s temp)
  (handler-case
      (prob:make-boltzmann-dist 
       (aref (table q) (item-number s (state-set q)))
       temp)
    (item-not-in-set ()
      (error 'unknown-state-action :state s :action nil :q-fn q))))



(defmethod reset ((q <tabular-q-function>) &optional (params nil))
  "reset all entries to 0."
  (assert (not params) () "params argument not supported for tabular-q-function")
  (loop
      for row across (table q)
      do (dotimes (j (length row))
	   (setf (aref row j) 0.0))))


(defmethod update ((q <tabular-q-function>) s a val eta)
  (handler-case
      (let* ((i (item-number s (state-set q)))
	     (j (item-number a (aref (action-sets q) i))))
	(avgf (aref (aref (table q) i) j) val eta))
    (item-not-in-set ()
      (error 'unknown-state-action :state s :action a :q-fn q))))
     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; other implemented methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod clone ((q <tabular-q-function>))
  "clones the table but not the sets."
  (make-tabular-q-function (clone (table q)) (state-set q) (action-sets q)))


(defmethod lin-alg:lp-dist ((q <tabular-q-function>) (q2 <tabular-q-function>) &optional (p 1))
  "TODO Should do error checking to see if state, action sets are equal."
  (expt
   (loop
       for r across (table q)
       for r2 across (table q2)
       sum (loop
	       for x across r
	       for y across r2
	       sum (expt (abs (- x y)) p)))
   (/ 1 p)))




