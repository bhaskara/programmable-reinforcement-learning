(defpackage resource-env
  (:use common-lisp
	utils
	stratagus-env
        create-env
  )
  (:export <resource-env>)
           *target-gold*
	   *target-wood*)
	   
	   
(in-package resource-env)

; the resource environment
; *target-gold* the target amount of gold to collect
; *target-wood* the target amount of wood to collect

(defvar *target-gold* 1000)
(defvar *target-wood* 500)

(defclass <resource-env> (<stratagus-env>) 
  ())

; the compute-reward function finds the amount of reward for each state
; for now, just -0.01 for each round

(defmethod compute-reward ((e <resource-env>) s a next-s)
  (+ (+ (- (global-state-gold (strat-es-global next-s))
           (global-state-gold (strat-es-global s)))
        (- (global-state-wood (strat-es-global next-s))
           (global-state-wood (strat-es-global s))))
     -1))
  

; the state terminates when we have achieved the target gold and wood

(defmethod is-terminal-state ((e <resource-env>) s) 
      (and (>= (global-state-gold (strat-es-global s)) *target-gold*)
	   (>= (global-state-wood (strat-es-global s)) *target-wood*)))

; reset procedure has to be extended to set elapsed-cycles to 0 upon reset

(defmethod reset :after ((e <resource-env>) &optional (reset-to-nonterminal-state t) &aux (s (stratagus-env::stratagus-env-game-socket e))))