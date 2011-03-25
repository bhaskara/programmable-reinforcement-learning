;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data-struct/pot-set/undirected-graphical-model.lisp
;; Defines the <undirected-graphical-model> class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package pot-set)

(defclass <undirected-graphical-model> (<prob-dist>)
  ((potentials :type list :initarg :potentials :reader potentials)
   (elim-order :type list :initarg :elim-order :reader elim-order)
   (inst-set :initarg :inst-set :reader inst-set))
  (:documentation "Class <undirected-graphical-model> (<prob-dist>)

Create using make-instance with initargs
:potentials - list of potentials
:elim-order - list of variables in order of elimination
:inst-set - <prod-set> representing set of instantiations to the variables
"))


;; todo prob

(defmethod sample ((gm <undirected-graphical-model>))
  (let* ((inst-set (inst-set gm))
	 (acc (inst-acc inst-set)))
    (eliminate-and-backsubstitute
     (potentials gm)
     (elim-order gm)
     inst-set
     *plus*
     *times*
     #'(lambda (pots j)
	 (let ((s (aref (sets inst-set) j)))
	   #'(lambda (inst)
	       (set:item  
		(sample
		 (make-multinomial-dist 
		  (mapset 'vector
			  #'(lambda (val)
			      (set-var-val acc inst j val)
			      (eval-pots pots *times* inst))
			  s)))
		s)))))))
	    
   
  
(defun boltzmann-gm (potentials elim-order s temp)
  "boltzmann-gm POTENTIALS ELIM-ORDER INST-SET TEMP

POTENTIALS - list of potentials
ELIM-ORDER - list of variables in order of elimination.
INST-SET - object of type <prod-set> that represents set of joint instantiations.
TEMP - the temperature.  Must be a nonnegative real number or the symbol 'utils:infty

Returns an object of type <undirected-graphical-model> (and therefore, of type [prob-dist]) representing the Boltzmann distribution over assignments, in which the probability of an assignment is proportional to exp(POT-TOTAL/TEMP) where POT-TOTAL is the sum of the potentials evaluated at the assignment.  If TEMP is 'infty, return a uniform distribution.  If TEMP is 0, returns a distribution that deterministically equals some maximizing assignment."
  
  (cond
   ((eql temp 'infty) (prob:make-unif-dist-over-set s))
   ((eql temp 0) (make-deterministic-dist (best-assignment *max* *plus* potentials elim-order s)))
   (t ;; temp is a positive real number
    (assert (and (numberp temp) (> temp 0)) (temp)
      "Illegal temperature ~a in Boltzmann distribution" temp)
    (let ((exp-fn #'(lambda (x) (exp (/ x temp)))))
      (make-instance '<undirected-graphical-model>
	:potentials (mapcar #'(lambda (pot) (compose-potential exp-fn pot)) potentials)
	:inst-set s
	:elim-order elim-order)))))

