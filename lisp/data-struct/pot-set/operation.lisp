;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data-struct/pot-set/operation.lisp
;; binary operations for the underlying semiring of a potential set
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package pot-set)

(defstruct (operation (:conc-name op-) (:constructor make-op (identity func)))
  "Structure type operation.  Represents a binary operation on a semiring, used by the graphical model code.  To create, use make-op with arguments
:identity - the identity element
:func - a function of two arguments.

See also the particular instances *times*, *plus*, *max*, and *min*."
  identity
  func)

(defun operate (op a b)
  "operate OP A B.  Apply OP of type OPERATION to A and B."
  (funcall (op-func op) a b))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some common operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *times* (make-op 1 #'*)
  "Structure of type operation representing multiplication of real numbers.")

(defparameter *plus* (make-op 0 #'+)
  "Structure of type operation representing addition of real numbers.")

(defparameter *max* (make-op '-infty #'mymax)
  "Structure of type operation representing maximization of extended real numbers.")

(defparameter *min* (make-op 'infty #'mymin)
  "Structure of type operation representing minimization of extended real numbers.")
