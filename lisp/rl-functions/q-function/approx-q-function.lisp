(in-package q-function)




(defvar *default-featurizer*
    #'(lambda (omega u)
	(cons (canonicalize omega) (canonicalize u))))


(defclass <approx-q-function> (<q-function>)
  ((fn-approx :type fn-approx:<fn-approx>
	      :reader fn-approx
	      :writer set-fn-approx
	      :initform (make-instance 'fn-approx:<tabular-fn-approx>)
	      :initarg :fn-approx)
   (featurizer :type function
	       :reader featurizer
	       :writer set-featurizer
	       :initarg :featurizer
	       :initform *default-featurizer*
	       :documentation "By default, features are computed by applying this function.")
   (featurizer-global-name :type symbol
			   :reader featurizer-name
			   :writer set-featurizer-name
			   :initform nil
			   :initarg :featurizer-name))
  (:documentation "Class <approx-q-function>.  Subclass of <q-function> that uses an object of type <fn-approx> to represent the Q-function, and doesn't assume any structure in the set of available choices at a state.

Implemented methods (all just pass the message on to the function-approximator)
- evaluate
- update
- reset
- clone

Initargs
:fn-approx - the function approximator.  Defaults to a tabular function approximator based on #'equalp.
:featurizer - function of two arguments, state and choice, that returns an object representing the features for this state and choice.  Defaults to #'canonicalize.
:featurizer-name - a symbol, such there is a globally defined variable with this name that evaluates to the featurizer function.  If not provided, and featurizer equals the default featurizer, then it is automatically set.  Otherwise, it is left unset.  In the latter case, the object can not be written to a stream in a readable format.  To read the object back from the stream, it must be the case that the featurizer is still defined (so you can't save a Q-function to a file, and then just restart Lisp and try to read it in - you have to load the Alisp system code and the code that defines the featurizer first).

To use this class, make a subclass of <approx-q-function> which implements the following methods
choices
featurize"))

(defmethod initialize-instance :after ((q <approx-q-function>) &rest args)
  (when (equal (featurizer q) *default-featurizer*)
    (set-featurizer-name '*default-featurizer* q)))
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implemented methods from <q-function>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod update ((q-fn <approx-q-function>) omega u target eta)
  (fn-approx:update (fn-approx q-fn) (featurize q-fn omega u) target eta))


(defmethod reset ((q-fn <approx-q-function>) &optional (params nil params-supplied))
  (let ((fa (fn-approx q-fn)))
    (if params-supplied
	(fn-approx:reset fa params)
      (fn-approx:reset fa))))


(defmethod evaluate ((q-fn <approx-q-function>) omega u)
  (handler-case
      (fn-approx:evaluate (fn-approx q-fn) (featurize q-fn omega u))
    (fn-approx:unknown-item ()
      (error 'unknown-state-action :state omega :action u
	     :q-fn q-fn))))
      

(defmethod copy-into progn ((q1 <approx-q-function>) (q2 <approx-q-function>))
  (set-fn-approx (clone (fn-approx q1)) q2)
  (set-featurizer (clone (featurizer q1)) q2)
  (set-featurizer-name (featurizer-name q1) q2))



(defun print-approx-q-function-readably (q-fn str)
  "print-approx-q-function-readably Q-FN STREAM

Print to STREAM a string that, when read, will result in the creation of a new object of the same type as Q-FN, which also has the same function approximator and featurizer as Q-FN.  For this to work, the featurizer-name field of Q-FN has be non-nil (if Q-FN was created using the default featurizer, this will happen automatically), and all the subcomponents have to themselves be printable readably."
  (let ((name (featurizer-name q-fn)))
	(assert name nil 
	  "Cannot print <approx-q-fn> readably, because its featurizer name is not available.")
	(format str "#.(make-instance '~W :featurizer ~W :featurizer-name '~:*~W :fn-approx "
		(type-of q-fn) name)
	(write (fn-approx q-fn) :stream str)
	(format str ")")))

(defmethod print-object ((q-fn <approx-q-function>) str)
  (if *print-readably*
      (progn
	(check-exact-class q-fn '<approx-q-function>)
	(print-approx-q-function-readably q-fn str))
     
    (print-unreadable-object (q-fn str :type t :identity nil)
      (format str "based on function approximator ~W" (fn-approx q-fn)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic functions to be implemented by subclasses
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric featurize (q-fn omega u)
  (:documentation "featurize APPROX-Q-FN OMEGA U.  Return the ``feature-vector'' (which need not actually be a Lisp vector) for this state and choice, which is then passed to the function approximator.  The default method just calls the function object stored in the featurizer slot of q-fn (which in turn, by default returns (canonicalize (cons omega u))).")
  (:method ((q-fn <approx-q-function>) omega u) (funcall (featurizer q-fn) omega u)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other useful methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun get-designated-q-feature (f)
  "get-designated-q-feature FEATURE-DESIGNATOR

Get the designated feature.  FEATURE-DESIGNATOR can be one of
- A constant C, denoting (constantly C)
- A function F of one argument, denoting (lambda (s a) (funcall f s))
- A function F of two arguments, denoting itself."
  
  (if (functionp f)
      (let ((l (get-lambda-list f)))
	(case (length l)
	  (1 (lambda (s a) (declare (ignore a)) (funcall f s)))
	  (2 f)
	  (otherwise (error "Lambda list ~a for q-feature does not have length 1 or 2" l))))
    (constantly f)))
  



(defun make-q-featurizer (&rest args)
  "make-q-featurizer [OUTPUT-TYPE] &rest FEATURES.

OUTPUT-TYPE - Either 'vector or 'list.  Defaults to 'vector.
FEATURES - List of designators for Q-features.  See (help get-designated-q-feature) for details."
  (let ((first-arg (first args)))
    (condlet
     (((member first-arg '(list vector)) (output-type first-arg) (features (rest args)))
      (t (output-type 'vector) (features args)))
     (let ((actual-features 
	    (mapcar #'get-designated-q-feature features)))
       (lambda (s a)
	 (map output-type
	   (lambda (f) (funcall f s a))
	   actual-features))))))

    




