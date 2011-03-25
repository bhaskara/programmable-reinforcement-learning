;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rl-functions/policy/tabular-policy.lisp
;; defines the <tabular-policy> class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package policy)

(defclass <tabular-policy> (<policy>)
  ((state-set :initarg :state-set
	      :reader state-set
	      :type set:[numbered-set])
   (table :initarg :table
	  :reader table
	  :type array))
  (:documentation "A <tabular-policy> has initargs
:state-set
:table

and represents a (deterministic, stationary) policy by a vector of actions.  Indexing is done using the numbered-set state-set."))

(defun make-tabular-policy (table &optional (state-set (length table)))
  (make-instance '<tabular-policy>
    :state-set state-set :table table))

(defmethod make-choice ((pol <tabular-policy>) s)
  (aref (table pol) (set:item-number s (state-set pol))))


(defmethod same ((p1 <tabular-policy>) (p2 <tabular-policy>))
  "Doesn't check sameness of state sets - just looks at the tables"
  (same (table p1) (table p2)))

(defmethod print-object  ((p1 <tabular-policy>) str)
  (format str "<<Tabular policy with table ~a>>" (table p1)))


