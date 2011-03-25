;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data-struct/pot-set/number-potential.lisp
;; Treating numbers as (constant) potentials
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package pot-set)

(defmethod vars ((pot number))
  nil)

(defmethod eval-pot ((pot number) inst)
  (declare (ignore inst))
  pot)

(defmethod compose-potential (fn (pot number))
  (funcall fn pot))