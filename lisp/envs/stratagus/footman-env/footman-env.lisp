;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; footman-env.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage footman-env
  (:use common-lisp
	utils
	stratagus-env
        create-env
  )
  (:export <footman-env>))
	   
	   

(in-package footman-env)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; class
;;
;; right now the constructor takes no parameters.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass <footman-env> (<stratagus-env>) 
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
;; Cost is only paid when entering the terminal state, and in that case equals the amount
;; gold used during the episode, divided by *gold-norm*
(defmethod compute-reward ((e <footman-env>) s a next-s)
  (declare (ignore a s))
  (if (is-terminal-state e next-s)
    (let ((my-units 0)
	  (enemy-units 0))
      (maphash #'(lambda(key value)
        (if (and (attack-unit? value) (= (unit-player-id value) *player*) 
            (> (unit-hp value) 0))
          (setf my-units (+ my-units 1))
          (if (and (attack-unit? value) (= (unit-player-id value) *enemy*) 
              (> (unit-hp value) 0))
            (setf enemy-units (+ enemy-units 1))))) (strat-es-units next-s))
      (- (* my-units 10) (* enemy-units 10)))
    -.1))

(defun compute-reward-test (s a next-s)
  (declare (ignore a s))
  (if (is-terminal-state-test next-s)
    (let ((my-units 0)
	  (enemy-units 0))
      (print "inner called")
      (maphash #'(lambda(key value)
        (if (and (attack-unit? value) (= (unit-player-id value) *player*) 
            (> (unit-hp value) 0))
          (setf my-units (+ my-units 1))
          (if (and (attack-unit? value) (= (unit-player-id value) *enemy*) 
              (> (unit-hp value) 0))
            (setf enemy-units (+ enemy-units 1))))) (strat-es-units next-s))
      (- (* my-units 10) (* enemy-units 10)))
    -.1))

;; is-terminal-state s
;; A state is terminal when either the enemy is dead, or the gold amount equals 0, or when too many peasants have been built
;(defmethod is-terminal-state ((e <strat-env1>) s)
;  (let* ((gold-mine (get-unit-state *gold-mine* s))
;	 (enemy (get-unit-state *enemy* s))
;	 (player-gold (global-state-gold (strat-es-global s))))
;    (or
;     (= (+ (unit-r-amt gold-mine) player-gold) 0)
;     (null enemy)
;     (>= (hash-table-count (strat-es-units s)) *max-num-units*)
;     (= (unit-hp enemy) 0))))


(defmethod is-terminal-state ((e <footman-env>) s)
  (let ((my-units 0)
	(enemy-units 0))
;    (print "DEBUG: footman-env::is-terminal-state called")
    (maphash #'(lambda(key value)
      (if (and (attack-unit? value) (= (unit-player-id value) *player*) 
          (> (unit-hp value) 0))
        (setf my-units (+ my-units 1))
        (if (and (attack-unit? value) (= (unit-player-id value) *enemy*) 
            (> (unit-hp value) 0))
          (setf enemy-units (+ enemy-units 1))))) (strat-es-units s))
    (or (= my-units 0) (= enemy-units 0))))

(defun is-terminal-state-test (s)
  (let ((my-units 0)
	(enemy-units 0))
    (maphash #'(lambda(key value)
      (if (and (attack-unit? value) (= (unit-player-id value) *player*) 
          (> (unit-hp value) 0))
        (setf my-units (+ my-units 1))
        (if (and (attack-unit? value) (= (unit-player-id value) *enemy*) 
            (> (unit-hp value) 0))
          (setf enemy-units (+ enemy-units 1))))) (strat-es-units s))
    (print my-units)
    (print enemy-units)
    (or (= my-units 0) (= enemy-units 0))))

(defun attack-unit? (u)
  (if (= (unit-type u) 0) ; footman
    t
    nil))

(defmethod effectors (s)
  (units-belonging-to-of-type 0 0 s))

(in-package common-lisp-user)





