(in-package alisp)

(defclass <alisp-approx-q-function> (q-function:<approx-q-function>)
  ()
  (:documentation "Subclass of <approx-q-function>.  Implements the choices method.  To make an ALisp Q-function for a particular domain, subclass this and implement the featurize method, and make sure the fn-approx slot gets set to some <fn-approx> object.  See documentation for <approx-q-function>

The default value for fn-approx is a tabular-fn-approx with all the default parameters used (see <tabular-fn-approx> documentation).
"))


(defmethod q-fn:choices ((q-fn <alisp-approx-q-function>) omega)
  (js-choices omega))


(defmethod print-object ((q-fn <alisp-approx-q-function>) str)
  (if *print-readably*
      (progn
	(check-exact-class q-fn '<alisp-approx-q-function>)
	(q-fn:print-approx-q-function-readably q-fn str))
    (call-next-method)))
