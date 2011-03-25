;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; alisp/q-function/features.lisp
;; define macros for creating features,
;; commonly used features
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(in-package alisp)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; commonly used features
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun constant-feature (&rest args)
  "constant-feature.  Returns 1 always."
  (declare (ignore args))
  1)

(defun choice (omega u)
  "choice OMEGA U.  Feature that returns the choice U."
  (declare (ignore omega))
  u)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macro for making features that condition on choice point
;; label
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro make-state-choice-feat-vec (options &rest unexpanded-defs)
  "macro make-state-choice-feat-vec OPTIONS &rest DEFS.  Return a function of two variables.  This function returns a vector of features based on the definitions.

As an example, (make-state-choice-feat-vec (foo bar baz oof) (qux quux quuux) will return a function of omega and u that returns an array of length 5.  If the label of the choice point that omega is at is 'foo, then the first element of the array will be the value function #'bar applied to omega and u.  Similarly, the second and third elements will be gotten by applying #'baz and #'oof.  The fourth and fifth elements will be 0.  On the other hand, if the label of the choice point is 'qux, then the first three elements of the returned array will be 0, and the fourth and fifth elements will be obtained by applying #'quux and #'quuux (respectively) to omega and u.

Note that this shouldn't usually be used when doing tabular function approximation, because the #'equal function, which is used to compare keys, does not recursively descend vectors.  In that case, use make-state-choice-feat-list instead."
  
  (declare (ignore options))
  (let* ((defs (mapcar #'expand-def unexpanded-defs))
	 (num-cases (length defs))
	 (start-inds (make-array (1+ num-cases) :initial-element 0)))
    (loop	
	for i from 1
	for def in defs
	do (setf (aref start-inds i) (+ (aref start-inds (1- i)) (1- (length def)))))
    (with-gensyms (omega u vec)
      `(lambda (,omega ,u)
	 (let ((,vec (make-array ,(flast start-inds) :element-type 'float
				 :initial-element 0.0)))
	   (cond
	    ,@(map 'list (lambda (def ind) (make-cond-clause def ind omega u vec)) defs start-inds)
	    (t (assert nil nil "Unknown choice label ~a" (js-label ,omega))))
	   ,vec)))))

(defmacro make-state-choice-feat-list ((&key include-cp-labels include-feat-names) &rest unexpanded-defs)
  "macro make-state-choice-feat-list (&key INCLUDE-CP-LABELS INCLUDE-FEAT-NAMES) &rest DEFS.  Return a function of two variables.  This function returns a list of features.

The syntax is the same as make-state-choice-feat-vec.  The only difference is that the return type of the returned function is a list, making this suitable when feature vectors are going to be compared using #'equal (for example, when doing tabular function approximation)."
  
  
  (let ((defs (mapcar #'expand-def unexpanded-defs)))
    (with-gensyms (omega u)
      `(lambda (,omega ,u)
	 (list
	  ,@(mapcar
	     (lambda (def)
	       (let ((cp-calls
		      `(when (equal (js-label ,omega) ',(first def))
			 (list
			  ,@(mapcar (lambda (d) 
				      (let ((fn-call
					     (make-individual-fn-call d omega u)))
					(if include-feat-names
					    `(list ',d ,fn-call)
					  fn-call)))
				    (rest def))))))
		 (if include-cp-labels
		     `(cons ',(first def) ,cp-calls)
		   cp-calls)))
	     defs))))))
		  
		  
    
				
(defun expand-def (d)
  "takes something like (foo bar (direct-product (baz qux) (oof ooof))) and returns something like
(foo bar (fn (* baz oof)) (fn (* baz ooof)) (fn (* qux oof)) (fn (* qux ooof)))"
  (cons (first d)
	(loop
	    for item in (rest d)
	    if (symbolp item) collect item
	    else if (eq (first item) 'direct-product)
	    append (expand-direct-product (rest item)))))


(defun expand-direct-product (l)
  (mapcar (lambda (x) `(fn (* ,@x))) (dp-helper l)))

(defun dp-helper (l)
  (if (eql (length l) 1)
      (mapcar #'list (first l))
    (mapcan
     (lambda (x)
       (mapcar (lambda (y) (cons x y)) (dp-helper (rest l))))
     (first l))))
	  
  
(defun make-cond-clause (def ind omega u vec)
  `((equal (js-label ,omega) ',(first def))
    ,@(loop
	  for i from ind
	  for d in (rest def)
	  collect `(setf (aref ,vec ,i)
		     ,(make-individual-fn-call d omega u)))))

(defun make-individual-fn-call (d omega u)
  (if (symbolp d)
      `(,d ,omega ,u)
    `(funcall ,d ,omega ,u)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; features that only depend on env state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro def-env-state-feature (feat-name fn-name &optional (doc-string ""))
  (with-gensyms (omega u)
    `(defun ,feat-name (,omega ,u)
       ,doc-string
       (declare (ignore ,u))
       (,fn-name (js-env ,omega)))))





