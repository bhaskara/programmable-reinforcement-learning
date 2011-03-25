;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; concurrent-alisp/calisp-program.lisp
;;
;; Defines the <calisp-program> abstract class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package calisp)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type defs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <calisp-program> ()
  ((root-thread-id :initform 'root-thread :initarg :root-thread-id :reader root-thread-id)
   (root-function :initarg :root-fn :reader root-fn)
   (assign-effectors-fn :initarg :assign-effectors-fn :reader assign-effectors-fn)
   (choosing-thread-fn :initform 'all :reader choosing-thread-fn :initarg :choosing-thread-fn))
  (:documentation "Class <calisp-program>.  

Initargs
:root-fn - the function where the root thread begins execution
:root-thread-id - the id of the root thread.  Equals 'root-thread by default.
:assign-effectors-fn - function that takes in a joint state and the set of unassigned effectors and returns an alist of which thread each one should be added to.  By default, all new effectors are assigned to the root thread.
:choosing-thread-fn - how the set of choosing threads is selected.  By default, equals 'all.  The allowed values for now are 'all, 'highest-level-first, 'highest-level-single, or a function
"))

(deftype [calisp-program] ()
  "[calisp-program]s can either be a function or an object of type <calisp-program>."
  `(or <calisp-program> function))


(defmethod initialize-instance :after ((p <calisp-program>) &rest args)
  (set-if-unbound 'assign-effectors-fn p
		  (lambda (omega effectors)
		    (assign-all-to-thread omega effectors (root-thread-id p)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric start (part-prog)
  (:documentation "This is where execution begins."))

(defmethod start ((part-prog function))
  (funcall part-prog))

(defmethod start ((part-prog <calisp-program>))
  (funcall (root-fn part-prog)))

(defmethod root-thread-id ((p function))
  'root-thread)

(defgeneric assign-effectors (p omega effectors)
  (:documentation "assign-effectors P OMEGA EFFECTORS.  

P - calisp partial program
OMEGA - joint state
EFFECTORS - list of effectors.

Return a list of thread ids of the same length as EFFECTORS.")
  (:method ((p function) omega effectors)
	   (assign-all-to-thread omega effectors 'root-thread))
  (:method ((p <calisp-program>) omega effectors)
	   (funcall (assign-effectors-fn p) omega effectors)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; choosing threads
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric choosing-thread-ids (cpp omega)
  (:documentation "choosing-thread-ids CPP OMEGA.  Return list of ids of threads that are going to choose next in OMEGA.")
  (:method ((cpp function) omega)
	   (get-choosing-threads 'all omega))
  (:method ((cpp <calisp-program>) omega)
	   (get-choosing-threads (choosing-thread-fn cpp) omega)))

(defmethod get-choosing-threads ((choice-fn (eql 'all)) omega)
  (hash-table-select 
   (js-thread-states omega)
   (lambda (v) (eq (ts-status v) 'holding))))

(defmethod get-choosing-threads ((choice-fn function) omega)
  (funcall choice-fn omega))
  

(defun ordered-choosing-threads-function (order)
  "return a choosing-threads function that returns the list of all threads whose label is earliest in the ordering."
  (lambda (omega)
    (let* ((t-states (js-thread-states omega))
	   (threads-at-levels
	    (mapcar
	     #'(lambda (l)
		 (hash-table-select 
		  t-states
		  #'(lambda (ts)
		      (member (ts-label ts) l :test #'equal))))
	     order)))
      (find-if #'identity threads-at-levels))))

(defmethod get-choosing-threads ((choice-fn (eql 'highest-level-first)) omega)
  (loop
      with thread-states = (js-thread-states omega)
      with best-level = nil
      with choosing-threads = nil
			      
      for k being each hash-key in thread-states using (hash-value v)
      for depth = (ts-stack-depth v)
						       
      when (eq (ts-status v) 'holding)
      if (or (not best-level) (< depth best-level))
      do (setf best-level depth)
	 (setf choosing-threads (list k))
      else if (= depth best-level)
      do (push k choosing-threads)
      end ;; if
      end ;; when

      finally (return choosing-threads)))

(defmethod get-choosing-threads ((choice-fn (eql 'highest-level-single)) omega)
  (list (first (get-choosing-threads 'highest-level-first omega))))
    
	    


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; env accessors 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defmacro def-env-accessor (feature-name fn-name &optional (doc-string ""))
  "Macro def-env-accessor FEATURE-NAME FUNCTION-NAME &optional DOC-STRING.  Expands to a definition of a function of no arguments named FEATURE-NAME that applies FUNCTION-NAME to the current environment state of a concurrent ALisp program."
  `(defun ,feature-name ()
     ,doc-string
     (,fn-name (env-state))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; internal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun assign-all-to-thread (omega effectors id)
  (declare (ignore omega))
  (make-list (length effectors) :initial-element id))

