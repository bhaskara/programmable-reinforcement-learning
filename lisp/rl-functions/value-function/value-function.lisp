(defpackage value-function
  (:documentation "Package for value functions.

Types
-----
<value-fn>
<tabular-value-fn>

Constructors
------------
make-tabular-value-fn

Operations
----------
value")
  (:nicknames value-fn)
  (:use common-lisp)
  (:export 
   <value-fn>
   make-tabular-value-fn
   value))

(in-package value-function)


(defclass <value-fn> () 
  ()
  (:documentation "Class for value functions.  Only operation is value.  Note that <q-function> is a subclass of this."))


(defgeneric value (val-fn omega)
  (:documentation "value VALUE-FUNCTION STATE.  Return optimal value of this state."))


