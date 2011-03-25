;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; strat-env1.lisp
;; 
;; A strategic domain corresponding to the stratagus domain strategic.pud.  The task in this
;; domain is to gather gold, build a barracks, produce footmen, and use them to kill the enemy.
;; Let G be the amount of gold remaining in the single gold mine.  The episode terminates when
;; the enemy is dead, or when G=0 (this shouldn't happen in any reasonable policy).  
;; Upon reaching the terminal state, we pay a cost proportional to the amount of gold remaining.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage strat-env1
  (:use common-lisp
	utils
	stratagus-env)
  (:export <strat-env1>
	   do-action
	   reset))
	   
	   

(in-package strat-env1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; class
;;
;; right now the constructor takes no parameters.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass <strat-env1> (<stratagus-env>) 
  ())



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant *gold-mine* 6)
(defconstant *enemy* 3)
(defconstant *init-gold* 100000)
(defconstant *gold-norm* 1000)
(defconstant *townhall* 4)
(defconstant *max-num-units* 25)
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ops
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; compute-reward e s a next-s
;;
;; Cost is only paid when entering the terminal state, and in that case equals the amount
;; gold used during the episode, divided by *gold-norm*
(defmethod compute-reward ((e <strat-env1>) s a next-s)
  (declare (ignore a s))
  (if (is-terminal-state e next-s)
      (if (>= (hash-table-count (strat-es-units next-s)) *max-num-units*)
	  0
	10)
    -.1))



;; is-terminal-state s
;; A state is terminal when either the enemy is dead, or the gold amount equals 0, or when too many peasants have been built
(defmethod is-terminal-state ((e <strat-env1>) s)
  (let* ((gold-mine (get-unit-state *gold-mine* s))
	 (enemy (get-unit-state *enemy* s))
	 (player-gold (global-state-gold (strat-es-global s))))
    (or
     (= (+ (unit-r-amt gold-mine) player-gold) 0)
     (null enemy)
     (>= (hash-table-count (strat-es-units s)) *max-num-units*)
     (= (unit-hp enemy) 0))))



(in-package common-lisp-user)





