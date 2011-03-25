(in-package stratagus-env)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the overall env state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (strat-env-state (:conc-name strat-es-) (:constructor make-strat-es))
  "Structure strat-env-state.  
Constructor : make-strat-es
Fields :
global - Accessed using strat-es-global.  A structure of type global-state
units - Accessed using strat-es-units.  A hash table from unit id to structure of type unit"
  global
  units)

(defparameter *not-necessary-to-print* '(58))

(defmethod print-object ((s strat-env-state) str)
  (format str "<<Strat env state with~&Global state : ~a" (strat-es-global s))
  (maphash 
   (lambda (k v)
     (unless (member (unit-type v) *not-necessary-to-print*)
       (format str "~&Unit ~a : ~a" k v)))
   (strat-es-units s))
  (format str ">>"))

;;;;;;
;; changes to canonicalize
;;;;;;

(defmethod canonicalize (x) x)

;;; end changes ;;;;

(defun get-unit-state (id s)
  "get-unit-state ID S.  S is of type strat-env-state.  Returns a structure of type unit, or nil if unit doesn't exist."
  (gethash id (strat-es-units s)))

(defun units-belonging-to (id s)
  "units-belonging-to PLAYER-ID STRAT-ENV-STATE.  Return list of ids of units belonging to this player.  Only returns those units with HP > 0, to deal with the ghost units that stick around for a while after they die."
  (hash-table-select (strat-es-units s)
		     (lambda (us)
		       (and (= (unit-player-id us) id) (> (unit-hp us) 0)))))
(defun units-belonging-to-of-type (id type s)
  "units-belonging-to-of-type ID TYPE STATE.  returns all units of a particular type belonging to player id in STATE"
  (hash-table-select (strat-es-units s)
		     (lambda (us)
		       (and (= (unit-player-id us) id) (> (unit-hp us) 0) (= (unit-type us) type)))))

(defun get-unit-loc (id s)
  (unit-loc (get-unit-state id s)))

(defun all-units (s)
  "Returns ALL unit IDs on the map with hp > 0"
  (hash-table-select (strat-es-units s)
		     (lambda (us)
		       (or (= (unit-type us) 93) (> (unit-hp us) 0)))))

(defmethod effectors ((s strat-env-state))
  (sort (units-belonging-to *my-player-id* s) #'<))

     


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global-state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct global-state
  (gold :type fixnum)
  (wood :type fixnum)
  (map))


;; clone
;; g : global-state
;;
;; returns a fresh structure 
(defmethod clone ((g global-state))
  (copy-structure g))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unit states
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct unit
  player-id
  type
  loc
  hp
  r-amt
  status
  status-args
  kills)


;; clone
;; u : unit
;;
;; returns a fresh structure with all the fields copied.  
(defmethod clone ((u unit))
  (make-unit
   :player-id (unit-player-id u)
   :type (unit-type u)
   :loc (copy-list (unit-loc u))
   :hp (unit-hp u)
   :r-amt (unit-r-amt u)
   :status (unit-status u)
   :status-args (copy-list (unit-status-args u))
   :kills (unit-kills u)))
  



(in-package cl-user)
