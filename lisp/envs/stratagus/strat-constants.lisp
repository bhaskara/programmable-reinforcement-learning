(in-package stratagus-env)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; strat-constants.lisp
;; stratagus-related constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some macros for defining stratagus constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-now
 (defun make-const-name (sym prefix)
    (intern (concatenate 'string "*" (symbol-name prefix) "-" (symbol-name sym) "*"))))

  (defmacro def-strat-const (const-type a name num)
    "Macro def-strat-const CONST-TYPE A NAME NUM.  E.g. (def-strat-const unit *unit-names* grunt 1) will make a constant *unit-grunt* with value 1 and set (aref *unit-names* 1) to the string \"grunt\".  Make sure to also export the symbol *unit-grunt* (see stratagus-env-pkg.lisp) in this case."
    `(eval-now      
      (defconstant ,(make-const-name name const-type) ,num)
      (setf (aref ,a ,num) (string-downcase (symbol-name ',name)))))

  (defmacro def-strat-consts (const-type a &rest args)
    "Macro def-strat-consts CONST-TYPE A NAME1 NUM1 NAME2 NUM2 ...
Repeatedly calls def-strat-const."
    (cons 'eval-now
	    (loop
		with l = args

		for name = (first l)
		for num = (second l)
		      
		      
		while l
		collect
		  `(def-strat-const ,const-type ,a ,name ,num)
		do (setf l (cddr l)))))

(eval-now
 (defparameter *action-names* (make-array 10))
 (defparameter *unit-names* (make-array 105))
 (defparameter *status-names* (make-array 10))
 (defparameter *move-names* (make-array 4)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; actions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def-strat-consts action *action-names*
  noop 0
  stop 1
  move 2
  build 3
  attack 4
  repair 5
  harvest 6
  dropoff 7)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unit types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(def-strat-consts unit *unit-names*
  footman 0
  grunt 1
  peasant 2
  peon 3
  ballista 4
  catapult 5
  knight 6
  ogre 7
  archer 8
  axethrower 9
  mage 10
  death-knight 11
  dwarves 14
  goblin-sappers 15
  human-oil-tanker 26
  orc-oil-tanker 27
  elven-destroyer 30
  troll-destroyer 31
  human-transport 28
  orc-transport 29
  battleship 32
  ogre-juggernaught 33
  gnomish-submarine 38
  giant-turtle 39
  gnomish-flying-machine 40
  goblin-zeppelin 41
  gryphon-rider 42
  dragon 43
  farm 58
  pig-farm 59
  human-barracks 60
  orc-barracks 61
  church 62
  altar-of-storms 63
  human-watch-tower 64
  orc-watch-tower 65
  elven-lumber-mill 76
  troll-lumber-mill 77
  stables 66
  ogre-mound 67
  gnomish-inventor 68
  goblin-alchemist 69
  gryphon-aviary 70
  dragon-roost 71
  mage-tower 80
  temple-of-the-damned 81
  human-refinery 84
  orc-refinery 85
  human-foundry 78
  orc-foundry 79
  human-shipyard 72
  orc-shipyard 73
  town-hall 74
  great-hall 75
  human-oil-platform 86
  orc-oil-platform 87
  keep 88
  stronghold 89
  castle 90
  fortress 91
  human-blacksmith 82
  orc-blacksmith 83
  human-wall 103
  orc-wall 104
  gold-mine 92
  oil-patch 93)
  

  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unit status ids
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-strat-consts status *status-names*
  undefined 0
  stop 1
  move 2
  attack 3
  build 4
  under-construction 5
  repair 6
  harvest 7)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unit costs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *gold-costs* (make-array 105 :element-type 'fixnum :initial-element -1))
(setf (aref *gold-costs* *unit-footman*) 600)
(setf (aref *gold-costs* *unit-peasant*) 400)
(setf (aref *gold-costs* *unit-human-barracks*) 700)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; movement directions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-strat-consts move *move-names*
  north 0
  east 1
  south 2
  west 3)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; convenience functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-cmd (&rest args)
  (if (= (length args) 1)
      (format nil "~a ~a" *action-build* (first args))
    (format nil "~a ~a ~a ~a" *action-build* (first args) (second args) (third args))))

(defun noop-cmd (&rest args)
  (format nil "~a" *action-noop*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; other constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant *my-player-id* 0)
(defconstant *command-message* "COMMAND")
(defconstant *ok-message* "OK")
(defconstant *unknown-message* "UNKNOWN")
(defconstant *ping-message* "PING")
(defconstant *pong-message* "PING")
(defconstant *request-gameinfo-message* "LISPGET GAMEINFO")
(defconstant *request-state-message* "LISPGET STATE")
(defconstant *request-map-message* "LISPGET MAP")
(defconstant *trans-message* "TRANS 50")
(defconstant *reset-message* "RESET")
(defconstant *noop* "NOOP")
(defconstant *print-inc* 100)




(in-package cl-user)