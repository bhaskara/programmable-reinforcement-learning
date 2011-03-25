;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rl-functions/value/tabular-value-function.lisp
;; Defines the <tabular-value-fn> class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package value-fn)

(defclass <tabular-value-fn> (<value-fn>)
  ((vals :initarg :vals
	 :reader vals
	 :type (simple-array float 1))
   (state-set :initarg :state-set
	      :reader state-set
	      :type set:[numbered-set]
	      ))
  (:documentation "A <tabular-value-fn> consists of a vector VALS of values, and a numbered set S.  The value of X is gotten by looking up the number of X in S, and getting the corresponding item from the vector VALS."))

(defun make-tabular-value-fn (v &optional (s (length v)))
  "make-tabular-value-fn VALS &optional STATE-SET.
Makes a <tabular-value-fn> having the given values.  If SET is not given, it is assumed to be the set 0,1,...,N-1 where N is the length of VALS."
  (make-instance '<tabular-value-fn> :vals v :state-set s))

(defmethod value ((v <tabular-value-fn>) x)
  (svref (vals v) (set:item-number x (state-set v))))

(defmethod print-object ((v <tabular-value-fn>) str)
  (format str "~&Tabular value function on set ~a with values ~a" (state-set v) (vals v)))
			 


(defmethod lin-alg:lp-dist ((v <tabular-value-fn>) (v2 <tabular-value-fn>) &optional (p 1))
  "TODO Should do error checking to see if state sets are equal."
  (lin-alg:lp-dist (vals v) (vals v2) p))