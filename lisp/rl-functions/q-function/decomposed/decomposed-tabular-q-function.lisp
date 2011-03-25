(in-package dec-q-fn)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; class def
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <decomposed-tabular-q-function> (<decomposed-q-function>)
  ()
  (:documentation "Class <decomposed-tabular-q-function> (<decomposed-q-function>)

A particular kind of decomposed Q-function, in which each component is a tabular q-function (using #'equalp for comparison).  If asked for a component it doesn't know about, it will just create a new tabular q-function."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; creation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod copy-into progn ((q <decomposed-tabular-q-function>) (q2 <decomposed-tabular-q-function>)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; exported operations are inherited from 
;; decomposed-q-function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; internal operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod make-new-q-component ((q <decomposed-tabular-q-function>) id)
  (declare (ignore id))
  (make-instance 'q-fn:<approx-q-function>
    :fn-approx (make-instance 'fn-approx:<tabular-fn-approx>)))

