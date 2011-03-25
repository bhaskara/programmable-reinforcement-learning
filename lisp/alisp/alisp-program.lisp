;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; alisp/alisp-program.lisp
;;
;; Defines the <alisp-program> abstract class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package alisp)



(defclass <alisp-program> ()
  ())

(deftype [alisp-program] ()
  "[alisp-program]s can either be a function or an object of type <alisp-program>."
  `(or <alisp-program> function))


(defgeneric start (part-prog)
  (:documentation "This is where execution begins."))

(defmethod start ((part-prog function))
  (funcall part-prog))



(defmacro def-env-accessor (feature-name fn-name &optional (doc-string ""))
  "Macro def-env-accessor FEATURE-NAME FUNCTION-NAME &optional (DOC-STRING "").  Expands to a definition of a function of no arguments named FEATURE-NAME that applies FUNCTION-NAME to the current environment state of an ALisp program."
  `(defun ,feature-name ()
     ,doc-string
     (,fn-name (env-state))))

  