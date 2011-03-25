(in-package dec-q-fn)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; class def
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <decomposed-crl-q-function> (<decomposed-q-function> crlq:<crl-q-function>)
  ((decomposed-templates :reader decomposed-templates :writer set-decomposed-templates
			 :initarg :decomposed-templates)
   )
   
  (:documentation "Class <decomposed-crl-q-function> (<decomposed-q-function> <crl-q-function>)

A particular kind of decomposed Q-function in which each component is a CRL Q-function, and the components share a weight vector.  Additionally, the overall Q-function also works like a CRL Q-function, in that it uses a coordination graph to pick the best choice at a state.

Initargs

:feature-ids - vector of names of feature templates

Provide at most one of
:decomposed-templates - a vector of decomposed templates, each of which is a function that takes in two arguments - a joint state and component id - and returns a designator for a list of features
:decomposed-template-descs - analogous to the parameter :template-descs of <crl-q-function> - a list of descriptions, each of which is a designator for a list of decomposed feature templates 


"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; creation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod copy-into progn ((q <decomposed-crl-q-function>) (q2 <decomposed-crl-q-function>))
  (set-decomposed-templates (decomposed-templates q) q2))

(defmethod shared-initialize :around ((q <decomposed-crl-q-function>) slots 
				      &rest args &key (decomposed-template-descs nil supplied)
						      (feature-ids (coerce 
								    (below (length decomposed-template-descs)) 
								    'vector)))
  
  ;; the around method parses the :decomposed-template-descs argument, if provided
  (if supplied
      (apply #'call-next-method q slots
	     :ids
	     (loop
		 with v = (make-array 0 :adjustable t :fill-pointer 0)
		 for id across feature-ids
		 for d in decomposed-template-descs
		 for n = (if (listp d) (length d) 1)
		 
		 if (= n 1)
		 do (vector-push-extend id v)
		 else
		 do (dotimes (i n)
		      (vector-push-extend (intern-compound-symbol id (format nil "~a" i)) v))
		 finally (return v))
	     :decomposed-templates 
	     (coerce (mapcan #'designated-list decomposed-template-descs) 'vector)
	     args)
    (call-next-method)))

(defmethod shared-initialize :after ((q <decomposed-crl-q-function>) slots &rest args)
  
  (declare (ignore slots args))
  ;; a decomposed-crl-q-function is, among other things, a crl-q-function, and must
  ;; have a set of feature templates, so next compute the set of templates from the 
  ;; set of decomposed feature templates and set the slot appropriately.
  (when (slot-boundp q 'decomposed-templates)
    (let ((comp-fn (component-fn q)))
      (crlq:set-temps 
       (map 'vector
	 #'(lambda (dec-temp)
	     #'(lambda (omega)
		 (reduce #'nconc
			 (funcall comp-fn omega)
			 :key #'(lambda (id)
				  (designated-list (funcall dec-temp omega id))))))
	 (decomposed-templates q))
       q))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; exported operations are inherited from crl-q-function
;; and decomposed-q-function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; internal operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod make-new-q-component ((q <decomposed-crl-q-function>) id)
  (make-instance 'crlq:<crl-q-function>
    :weights (crlq:weights q)
    :choice-fn (crlq:choice-fn q)
    :ids (crlq:ids q)
    :feature-templates
    (map 'vector
      #'(lambda (decomposed-temp)
	  (lambda (omega) (funcall decomposed-temp omega id)))
      (decomposed-templates q))))
