
(defpackage tactical-fc-env
  (:documentation "The tactical-fc-env package.  Defines <tactical-fc-env>, for the 'footmen and catapults' tactical environment.  In this environment, the player starts with a large amount of resources, and the ability to produce footmen and catapults.  The goal is to destroy the enemy great hall, which is protected by some units.  A cost-of-living is charged per timestep, and a reward of hp-reward is given for each unit of damage done to the great hall.")
  (:use s-env
	cl
	utils))


  
  

(in-package tactical-fc-env)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass <tactical-fc-env> (<stratagus-env>) 
  ((cost-of-living :documentation "Cost paid per timestep"
		   :reader cost-of-living
		   :initform .001
		   :initarg :cost-of-living)
   (damage-reward :documentation "Reward per unit of damage done to great hall"
		  :reader damage-reward
		  :initform .01
		  :initarg :damage-reward))
   (:documentation "Class <tactical-fc-env>.  Constructor parameters 
:cost-of-living (.001 by default)
:damage-reward (.01 by default)"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant *enemy-id* 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ops
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; is-terminal-state s
;; Episode terminates when the enemy's great hall is destroyed
(defmethod is-terminal-state ((e <tactical-fc-env>) s)
  (<= (great-hall-hp s) 0))

;; reward for doing damage to great hall minus cost-of-living 
(defmethod compute-reward ((e <tactical-fc-env>) s a s2)
  (declare (ignore a))
  (-
   (* (damage-reward e) (- (great-hall-hp s) (great-hall-hp s2)))
   (cost-of-living e)))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; internal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun great-hall-hp (s)
  "great-hall-hp STATE.  Hit points of enemy great hall in this state."
  (let ((gh (first (units-belonging-to-of-type *enemy-id* *unit-great-hall* s))))
    (if gh
	(unit-hp (get-unit-state gh s))
      0)))
	
  

  


(in-package common-lisp-user)





