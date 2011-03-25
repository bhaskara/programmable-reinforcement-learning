;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rl-functions/q-function/crl/crl-features.lisp
;; a bunch of functions to simplify the task of writing features and
;; feature templates for crl-q-functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package crlq)

(defun compose-feature (fn f)
  "compose-feature FUNCTION FEATURE.  Returns a new feature that depends on the same variables as FEATURE, and is computed by applying FUNCTION to the result of FEATURE."
  (compose-potential fn f))

(defun compose-feature-template (fn ft)
  "compose-feature-template FUNCTION FEATURE-TEMPLATE.  Returns a new feature template that first applies FEATURE-TEMPLATE to a state OMEGA, then composes FUNCTION (using the function compose-feature) with each of the returned features."
  (etypecase ft
    (function (lambda (&rest args)
		(mapcar
		 (lambda (feature)
		   (compose-feature fn feature))
		 (designated-list (apply ft args)))))
    (number (funcall fn ft))))


(defun bin-values (ft bin-spec)
  "bin-values FEATURE-TEMPLATE BIN-SPEC.

FEATURE-TEMPLATE is a feature template.
BIN-SPEC is a positive integer or sequence

Returns a list of feature templates. 

When BIN-SPEC is an integer, for each i between 0 and BIN-SPEC-1, there is a corresponding template that evaluates FEATURE-TEMPLATE and indicates whether the value of the resulting features equals I.  The feature also asserts if the value isn't an integer between 0 and BIN-SPEC - 1.

When BIN-SPEC is a sequence, it is assumed to be an increasing sequence of extended real numbers a1, ..., an (real numbers or 'utils:infty or 'utils:-infty).  In this case, there are n-1 features, corresponding to the intervals [a1, a2), ..., [a(n-1), an)"

  (etypecase bin-spec
    (integer (mapset 'list
		     (lambda (i)
		       (compose-feature-template
			(lambda (x) 
			  (assert (and (integerp x)
				       (between2 x 0 bin-spec))
			      ()
			    "Value ~a could not be binned as an integer between 0 and ~a-1" x bin-spec)
			  (indicator (eq x i)))
			ft))
		     bin-spec))
    (sequence (assert (is-sorted bin-spec #'my<) () "Bin-spec ~a must be sorted" bin-spec)
	      (let ((min (sfirst bin-spec))
		    (max (slast bin-spec))
		    (bin-spec (coerce bin-spec 'vector)))
	      
		(mapset 'list
			(lambda (i)
			  (compose-feature-template
			   (lambda (x)
			     (assert (between2 x min max) ()
			       "~a not between ~a and ~a, so could not be binned" x min max)
			     (indicator (between2 x (aref bin-spec i) (aref bin-spec (1+ i)))))
			   ft))
			(1- (length bin-spec)))))))



