(defpackage decomposed-q-function
  (:documentation "Package decomposed-q-function (dec-q-fn).

A decomposed q-function is a sum of a set (which can vary with state) of other q-functions.

Types
-----
<decomposed-q-function>
<decomposed-tabular-q-function>
<decomposed-crl-q-function>

Accessors
---------
set-component-fn
component-fn

Operations
----------
get-q-component
update-component
evaluate-component
evaluate

")
  (:nicknames dec-q-fn)
  (:use
   q-fn
   cl
   set
   utils)
  (:export
   <decomposed-q-function>
   <decomposed-tabular-q-function>
   <decomposed-crl-q-function>
   component-fn
   set-component-fn
   get-q-component
   update-component
   evaluate-component
   
   evaluate
   ))

(in-package dec-q-fn)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; class def
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <decomposed-q-function> (<q-function>)
  ((component-fn :type function :reader component-fn :initarg :component-fn :writer set-component-fn)
   (q-function-table :accessor q-fn-table :initform (make-hash-table :test #'equal)))
  (:documentation "Represents a q-function which works as follows.  At a state omega, a 'component function' is first applied to omega and returns a set of component ids.  Next, each id is looked up in an #'equal hash-table to return a q-function object.  Finally, the resulting q-function objects are applied to the given omega, u and added together."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod copy-into progn ((q <decomposed-q-function>) (q2 <decomposed-q-function>))
  (set-component-fn (component-fn q) q2)
  (setf (q-fn-table q2) (clone (q-fn-table q))))

(defmethod evaluate ((q <decomposed-q-function>) omega u)
  (set:sum-over 
   (funcall (component-fn q) omega)
   #'(lambda (id) 
       (handler-bind
	   ((unknown-state-action #'(lambda (c) (declare (ignore c)) (use-value 0))))
	 (evaluate-component q id omega u nil)))
	 
   ))

(defun update-component (q id omega u target eta)
  "update-component DECOMPOSED-Q-FUNCTION ID STATE CHOICE TARGET STEP-SIZE
Update the q-function corresponding to the given component to be more like TARGET at STATE, CHOICE."
  (let ((comps (funcall (component-fn q) omega)))
    (mvbind (pres reason) (member? id comps)
      (assert pres ()
	"Component ~a not present in component list ~a because ~a"
	id comps (get-explanation-string reason))))
  (update (get-q-component q id) omega u target eta))

(defun evaluate-component (q id omega u &optional (check-legal-component t))
  "evaluate-component DECOMPOSED-Q-FUNCTION COMPONENT-ID STATE CHOICE &optional (CHECK-LEGAL? t)
Evaluate the given component.  If CHECK-LEGAL? is true, then first make sure COMPONENT-ID is a valid component at STATE.

The evaluation of the Q-component may result in an unknown-state-action error being signalled."

  (when check-legal-component
    (let ((comps (funcall (component-fn q) omega)))
      (mvbind (pres reason) (member? id comps)
	(assert pres ()
	  "Component ~a not present in component list ~a because ~a"
	  id comps (get-explanation-string reason)))))
  
  (evaluate (get-q-component q id) omega u))


(defun get-q-component (q id)
  "get-q-component DECOMPOSED-Q-FUNCTION ID.  Get the component with label ID."
  (let ((table (q-fn-table q)))
    (handler-case
	(mapping:evaluate table id)
      (mapping:mapping-undefined ()
	(setf (mapping:evaluate table id)
	  (make-new-q-component q id))))))


(defgeneric make-new-q-component (q id)
  (:documentation "A way for subclasses of <decomposed-q-function> to customize how objects corresponding to a particular Q-component are created.  This function is called when the get-q-component function fails to find the Q-function corresponding to a particular ID in the table."))


(defvar *dec-q-fn-comp-print-width* 20)

(defmethod policy:print-advice ((q <decomposed-q-function>) omega choices str)
  ;; called when using a prompt-policy with this q-function as an advisor
  ;; (this happens when using io-interface mode for flat rl, alisp, or calisp)
  
  (format str "Decomposed Q-function~%")
  (without-quotes
   (pprint-logical-block (str nil)
     (do-elements (id (funcall (component-fn q) omega))
       (format str "~:@_~W~2,V:@T" id *dec-q-fn-comp-print-width*)
       (handler-case
	   (let ((comp (mapping:evaluate (q-fn-table q) id)))
	     (pprint-fill str
			  (mapset 'list 
				  #'(lambda (u) 
				      (cons u 
					    (handler-case
						(q-fn:evaluate comp omega u)
					      (q-fn:unknown-state-action ()
						'unknown))))
				  choices) nil))
	 (mapping:mapping-undefined ()
	   (format str "Component undefined in this Q-function." id)))))))
  
  
  
