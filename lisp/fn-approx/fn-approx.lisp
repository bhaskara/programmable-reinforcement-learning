(defpackage fn-approx 
  (:documentation "Contains code for representing parametrized function approximators.

Types
-----
<fn-approx>
<tabular-fn-approx>
<linear-fn-approx>
<linear-fn-approx-with-bounds>
<array-fn-approx>

Conditions and methods on them
------------------------------
unknown-item
unknown-item-evaluate
unknown-item-update
fn-approx


Externally callable (these should be implemented by subclasses)
---------------------------------------------------------------
reset 
update 
evaluate 
get-params (optionally)
clone
print-object (this should be implemented to respect *print-readably*)

For subclasses
--------------
set-params
params")

  (:use common-lisp
	utils)
  (:export <fn-approx>
	   <tabular-fn-approx>
	   <linear-fn-approx>
	   <linear-fn-approx-with-bounds>
	   <array-fn-approx>
	   unknown-item
	   unknown-item-evaluate
	   unknown-item-update
	   fn-approx
	   reset
	   update
	   evaluate
	   params
	   get-params
	   clone
	   set-params))


(in-package fn-approx)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass <fn-approx> ()
  ((params :reader params
	   :writer set-params
	   :documentation "current parameters of function approximation"
	   :initarg :params)))


(define-condition unknown-item ()
  ((item :reader unknown-item :initarg :item)
   (fn-approx :reader fn-approx :initarg :fn-approx))
  (:documentation "Signalled when an unknown item is added or updated. Fields are unknown-item and fn-approx."))

(define-condition unknown-item-evaluate (unknown-item) ())

(define-condition unknown-item-update (unknown-item) ())

(defmethod print-object ((c unknown-item-update) str)
  (format str "<<Condition for unknown item update ~a in function approximator>>" (unknown-item c)))

(defmethod print-object ((c unknown-item-evaluate) str)
  (format str "<<Condition for unknown item evaluate ~a in function approximator>>" (unknown-item c)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; externally called methods (which must be implemented
;; by subclasses)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric reset (fa &optional theta)
  (:documentation "reset FA &optional THETA.  Reset the parameters of FA, to value THETA if it is provided, or to a default initial value if it is not."))

(defgeneric update (fa x y eta)
  (:documentation "update FA X Y ETA.  Update the parameters of FA based on observing an example with input X and output Y, using learning rate ETA.  ETA ranges from 0 to 1 and larger ETA results in a bigger change in the parameters.  Might signal an unknown-item-update condition when a new X is added, but the condition need not be handled."))

(defgeneric evaluate (fa x)
  (:documentation "update FA X.  Evaluate the function represented by the current value of the parameters at input X.  Might signal an unknown-item-evaluate condition if seeing an unknown input, but the condition need not be handled."))

(defgeneric get-params (fa)
  (:documentation "get-params FA.  Return a fresh copy of the current parameters of FA.")
  (:method ((fa <fn-approx>))
	   (clone (params fa))) ;; should be overridden by children whose parameter object doesn't have a clone method
  )

(in-package cl-user)

	
