(defpackage variable-effector-env
  (:documentation "Package variable effector-env (ve-env).

A toy domain adapted from Stratagus, which provides an example of an environment where the set of effectors changes over time.

Types
-----
<ve-env>

State accessors
---------------
units
resources
enemy-hps
base-hp
peas-cost
footman-cost
damage-prob

Commands
--------
gather
train-peas
train-footman
wait
attack
defend

Other symbols
-------------
peasant
footman

")
  (:nicknames ve-env)
  (:use create-env
	cl
	utils
	prod-set
	inst-vars
	policy
	set)
  
  (:export
   <ve-env>
   units
   resources
   enemy-hps
   base-hp
   peas-cost
   footman-cost
   gather
   train-peas
   train-footman
   wait
   attack
   defend
   peasant
   footman
   ))

(in-package ve-env)


(defstruct (ve-env-state (:conc-name nil))
  enemy-hps
  (resources 0)
  peas-cost
  footman-cost
  units
  damage-prob
  base-hp)

(defmethod print-object ((s ve-env-state) str)
  (format str "<<Variable-effector-env state with units ~a, enemy hp ~a, resources ~a, and base hp ~a>>"
	  (units s) (enemy-hps s) (resources s) (base-hp s)))


(defclass <ve-env> (<fully-observable-env>)
  ((init-enemy-hps :reader init-enemy-hps :initform #(10 10) :initarg :init-enemy-hps)
   (peas-cost :reader get-peas-cost :initform 4 :initarg :peas-cost)
   (footman-cost :reader get-footman-cost :initform 6 :initarg :footman-cost)
   (damage-prob :reader get-damage-prob :initform .5 :initarg :damage-prob)
   (init-base-hp :reader init-base-hp :initform 1 :initarg :init-base-hp)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic env operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod sample-init ((e <ve-env>))
  (make-ve-env-state :enemy-hps (copy-seq (init-enemy-hps e))
		     :units #(peasant footman) :base-hp (init-base-hp e)
		     :peas-cost (get-peas-cost e) :footman-cost (get-footman-cost e)
		     :damage-prob (get-damage-prob e)))

(defmethod effectors ((s ve-env-state))
  (let ((units (units s)))
    (reverse
     (filter
      'list (1+ (length units))
      (lambda (i) (or (eq i 0) (not (eq (aref units (1- i)) 'dead))))
      ))))

(defmethod sample-next ((e <ve-env>) (s ve-env-state) action)
  (let ((actions (avail-actions e s)))
    (assert (member? action actions) ()
      "Action ~a is not a member of action set ~a"
      action actions))
  
  (let ((num-enemies (length (enemy-hps s)))
	(enemy-hps (enemy-hps s))
	(b (base-hp s))
	(units (units s))
	(new-res (resources s))
	new-units)
    
    (let ((attacking (make-array (1+ num-enemies) :initial-element nil))
	  (ehp (copy-seq enemy-hps)))
      
      
      ;; figure out how many footmen are attacking each enemy or defending
      (dolist (entry action)
	(let ((id (car entry)))
	  (when (> id 0) 
	    (ecase (aref units (1- id)) 
	      (footman (let ((cmd (cdr entry)))
			 (if (eql cmd 'defend)
			     (push id (aref attacking num-enemies))
			   (push id (aref attacking (second cmd))))))
	      (peasant (incf new-res))))))
      
      ;; figure out new number of units
      (let ((base-cmd (cdr (assoc 0 action)))
	    (num-units (length units)))
	(flet ((same-num-units (r)
		 (setf new-units (copy-seq units))
		 (setf new-res r))
	       (add-unit (unit-type cost)
		 (setf new-units (make-array (1+ num-units))
		       (aref new-units num-units) unit-type
		       new-res (- new-res cost))
		 (dotimes (i num-units)
		   (setf (aref new-units i) (aref units i)))))
	  (ecase base-cmd
	    (train-peas (if (< new-res (get-peas-cost e))
			    (same-num-units 0)
			  (add-unit 'peasant (get-peas-cost e))))
	    (train-footman (if (< new-res (get-footman-cost e))
			       (same-num-units 0)
			     (add-unit 'footman (get-footman-cost e))))
	    (wait (same-num-units new-res)))))
      
      ;; figure out new base hp, enemy hps
      (loop
	  for i below num-enemies
	  for a across attacking
	  for h across enemy-hps
	  when (> h 0)
	  do (when a
		 (progn
		   (setf (aref ehp i)
		     (max 0 
			  (- (aref ehp i) (1- (length a)))))
		   (let ((dead (prob:sample-uniformly a)))
		     (when (< (random 1.0) (damage-prob s))
		       (setf (aref new-units (1- dead)) 'dead)))))
	     (unless (aref attacking num-enemies)
	       (setf b (max 0 (1- b)))))
      
      (values (make-ve-env-state :enemy-hps ehp :resources new-res
				 :base-hp b :units new-units
				 :peas-cost (peas-cost s) :damage-prob (damage-prob s)
				 :footman-cost (footman-cost s))
	      (if (> b 0)
		  -.1
		-20)))))


(defmethod policy:prompt-for-choice (s choices (e <ve-env>))
  (loop
      with action = nil
      do (format t "~&Enter action vector or nil to abort.  ")
	 (let ((a (read)))
	   (unless a
	     (error 'choose-to-abort))
	   (setf action
	     (if (listp a)
		 (if (symbolp (first a))
		     (mapcar #'cons (effectors s) a)
		   a)
	       (loop
		   for i in (effectors s)
		   for act across a
		   when act
		   collect (cons i act)))))
	 (if (member? action choices)
	     (return action)
	   (format t "~&Illegal action ~a for set ~a." action choices))))

(defmethod is-terminal-state ((e <ve-env>) s)
  (or
   (<= (base-hp s) 0)
   (every (lambda (x) (<= x 0)) (enemy-hps s))))

(defmethod avail-actions ((e <ve-env>) s)
  (let ((acc (make-alist-accessors (effectors s)))
	(footmen-actions
	 (cons 'defend
	       (mapset 'list (lambda (i) `(attack ,i))
		       (length (enemy-hps s))))))
    (make-instance '<prod-set> :inst-acc acc
		   :sets (cons '(train-peas train-footman wait)
			       (loop
				   for u across (units s)
				   when (eq u 'footman)
				   collect footmen-actions
				   when (eq u 'peasant)
				   collect '(gather))))))
					
