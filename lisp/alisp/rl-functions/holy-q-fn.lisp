(in-package alisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; class def
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <holy-q-function> (<q-function>)
  ((qr :type <q-function> :reader qr :initarg :qr)
   (qc :type <q-function> :reader qc :initarg :qc)
   (pe :reader pe :initarg :pe)
   (cache :reader cache :initarg :cache)
   (top-pred :type function :reader top-pred :documentation "Predicate that's true iff a state is at the top level, so has no exit distribution." :initform #'top-level-state? :initarg :top-pred))
  (:documentation "Hierarchically Optimal Yet Local Q-functions.  A Q-function representation based on the decomposition Q = Qr + Qc + Pe * V.  Use make-holy-q-fn to create."))


(defparameter *default-cache-size* 20)
(defun make-holy-q-fn (qr qc pe &key (cache-size *default-cache-size*))
  (make-instance '<holy-q-function> :qr qr :qc qc :pe pe
		 :cache (make-instance 'cache:<fifo-cache> 
			  :size cache-size :test #'same)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; implemented operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod q-fn:evaluate ((q-fn <holy-q-function>) omega u)
  ;; TODO - quite inefficient and should be memoized in case
  ;; multiple calls are being made for different OMEGA and U 
  (handler-case
      (+ 
       (q-fn:evaluate (qr q-fn) omega u)
       (q-fn:evaluate (qc q-fn) omega u)
       (if (funcall (top-pred q-fn) omega)
	   0
	 (expectation
	  (cond-dist (pe q-fn) (cons omega u))
	  (lambda (omega2) (q-fn:value q-fn omega2)))))
    (unknown-state-action ()
      (error 'unknown-state-action :state omega :action u :q-fn q-fn))))
	

  
  

(defmethod q-fn:reset ((q-fn <holy-q-function>) &optional (params nil params-supplied))
  (declare (ignore params))
  (assert (not params-supplied))
  (q-fn:reset (qr q-fn))
  (q-fn:reset (qc q-fn))
  (reset-cache q-fn)
  (reset (pe q-fn)))

(defmethod q-fn:value :around ((q-fn <holy-q-function>) omega)
  ;; first check the cache
  (let ((c (cache q-fn)))
    (multiple-value-bind (val pres)
	(cache:lookup omega c)
      (if pres
	  val
	(let ((v (call-next-method)))
	  (progn
	    (cache:add omega v c)
	    v))))))




(defmethod clone ((q-fn <holy-q-function>))
  (make-instance '<holy-q-function>
    :qr (clone (qr q-fn)) 
    :qc (clone (qc q-fn))
    :pe (clone (pe q-fn))
    :cache (make-instance 'cache:<fifo-cache> :size (cache:size (cache q-fn)) :test #'same)
    :top-pred (top-pred q-fn)))


(defmethod q-fn:choices ((q-fn <holy-q-function>) omega)
  ;; doesn't bother to check that the various components agree on what the choice set is
  (js-choices omega))

(defun evaluate-holy-q-comps (q omega u)
  "evaluate-holy-q-comps Q OMEGA U.  Return 3 values - 1) Qr(omega, u) 2) Qc(omega, u) 3) Pe(_|omega, u)"
  (values
   (q-fn:evaluate (qr q) omega u)
   (q-fn:evaluate (qc q) omega u)
   (cond-dist (pe q) (cons omega u))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; debug
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *rounding* 3)
(defparameter *prob-rounding* 2)

(defmethod policy:print-advice ((q <holy-q-function>) omega choices str)
  (handler-bind ((q-fn:unknown-state-action 
		  #'(lambda (c)
		      (declare (ignore c))
		      (use-value 'unknown))))
    (set:do-elements (u choices)
      (format str "~&Choice ~a.  Q = ~a  " u (round-decimal (q-fn:evaluate q omega u) *rounding*))
      (format str "Qr = ~a  " (round-decimal (q-fn:evaluate (qr q) omega u) *rounding*))
      (format str "Qc = ~a  Pe*V = " (round-decimal (q-fn:evaluate (qc q) omega u) *rounding*))
      (let ((d (cond-dist (pe q) (cons omega u))))
	(if (eq (caar d) 'aed::dummy-unknown-state)
	    (format str "unknown dist")
	  (let ((first t))
	    (prob:do-sample-points (x p (cond-dist (pe q) (cons omega u)))
	      (if first (setf first nil) (format str " + "))
	      (format str "~a*~a" (round-decimal p *prob-rounding*)
		      (round-decimal (q-fn:value q x) *rounding*)))
	    (format str " = ~a"
		    (round-decimal
		     (expectation
		      (cond-dist (pe q) (cons omega u))
		      (lambda (omega2) (q-fn:value q omega2)))
		     *rounding*))))))
    (handler-case (format str "~&Recommended choice is ~a" (q-fn:best-choice q omega))
      (q-fn:unknown-state-action () (format str "~&Unable to determine best choice.")))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; internal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun top-level-state? (omega)
  "top-level-state? OMEGA - is OMEGA at the top level, i.e. is its stack empty?"
  (null (js-stack omega)))

(defun reset-cache (q-fn)
  (cache:reset (cache q-fn)))
