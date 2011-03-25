(in-package calisp)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; calisp q-functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <calisp-q-function> (q-fn:<q-function>)
  ()
  (:documentation "Q-function for concurrent ALisp programs.  Implements the choices method."))

(defmethod q-fn:choices ((q-fn <calisp-q-function>) omega)
  (js-choices omega))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; calisp q-functions based on a <fn-approx> object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <calisp-approx-q-function> (q-fn:<approx-q-function> <calisp-q-function>)
  ()
  (:documentation "CAlisp Q-function based on an arbitrary Q-function approximator.  Note that this is likely to be inefficient in general because maximization will be done by brute-force.  Subclasses <calisp-crl-q-function> are preferable if there will be many threads simultaneously running."))

(defmethod print-object ((q-fn <calisp-approx-q-function>) str)
  (if *print-readably*
      (progn
	(check-exact-class q-fn '<calisp-approx-q-function>)
	(q-fn:print-approx-q-function-readably q-fn str))
    (call-next-method)))


