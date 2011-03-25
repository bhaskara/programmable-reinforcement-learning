;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data-struct/pot-set/potential.lisp
;; operations on tabular potentials
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package pot-set)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defstruct (tabular-potential (:conc-name tab-pot-)
	    (:constructor create-tabular-potential))
  "Structure type tabular-potential.  Create using make-tabular-potential."
  vars
  table
  insts)

(defmethod vars ((pot tabular-potential))
  (tab-pot-vars pot))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-tabular-potential (s vars func)
  "make-tabular-potential SET VARS FUNC

SET - the overall product set of values for complete joint instantiations
VARS - the particular variables that this potential depends on
FUNC - a function that takes in an instantiation and returns the value of this potential on it, which must be a floating point number."

  (let ((insts (make-subspace s vars t)))
    (create-tabular-potential
     :insts insts
     :vars vars
     :table (mapset '(vector float) (lambda (x) (coerce (funcall func x) 'float)) insts))))

(defmethod eval-pot ((pot tabular-potential) inst)
  (let ((v (svref (tab-pot-table pot) (item-number inst (tab-pot-insts pot)))))
    (check-type v float)
    v))
