;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; adv-tact-env.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage adv-tact-env
  (:use common-lisp
	utils
	stratagus-env
        create-env
  )
  (:export <adv-tact-env>))
	   
	   

(in-package adv-tact-env)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; class
;;
;; right now the constructor takes no parameters.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass <adv-tact-env> (<stratagus-env>) 
  ())



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
(defconstant *player* 0)
(defconstant *enemy* 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ops
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; compute-reward e s a next-s
;;
;; victory cost is only paid when entering the terminal state, and in that case equals
;; -10 for defeat, 10 for victory; each step, reward is determined by the change in enemy
;; unit hps, but is at least -0.1 to account for time
 
(defmethod compute-reward ((e <adv-tact-env>) s a next-s)
  (declare (ignore a))
  (if (is-terminal-state e next-s)
    (let ((my-units 0)
	  (enemy-units 0))
         (if (> (units-belonging-to-except *enemy* next-s 59) 0)
            -10
            10))
    (+ -0.1 (* 0.001 (- (all-enemy-unit-hps s) 
            (all-enemy-unit-hps next-s))))))

;; is-terminal-state s
;; A state is terminal when either the enemy is dead, or the player is dead

(defmethod is-terminal-state ((e <adv-tact-env>) s)
  (or (= (list-length (units-belonging-to-except *enemy* s 59)) 0)
      (= (list-length (units-belonging-to-except *player* s 59)) 0)))

;;
;; support functions
;; 

(defun all-enemy-unit-hps (s)
  (let ((enemy-unit-hps 0))
    (maphash #'(lambda(key value)
      (if (= (unit-player-id value) *enemy*)
        (setf enemy-unit-hps (+ enemy-unit-hps (unit-hp value)))))
      (strat-es-units s))
  enemy-unit-hps))

(defun units-belonging-to-except (id s ty)
  "units-belonging-to-except PLAYER-ID STRAT-ENV-STATE TYPE.  Return list of ids of units belonging to this player except of the type specified by TYPE.  Only returns those units with HP > 0, to deal with the ghost units that stick around for a while after they die."
  (hash-table-select (strat-es-units s)
		     (lambda (us)
		       (and (< (unit-type us) 105) (not (= (unit-type us) ty)) (= (unit-player-id us) id) (> (unit-hp us) 0)))))

(in-package common-lisp-user)


