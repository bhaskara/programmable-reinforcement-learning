;; ASSUMES: At any time this is called, s consists of exactly one peasant thread
;; that is at a choice point, and a is that particular choice

(defun make-resource-featurizer ()
    (lambda (s a tid)
      (let ((subtask-label (js-thread-label s tid)))
        (cond 
	    ;; corresponds to peasant-top
	    ((equal subtask-label 'Root) (list 'tabular 'Root (root-tabular-features s a tid)))
            ((equal subtask-label 'Get-Gold) (list 'tabular 'Get-Gold (get-gold-tabular-features s a tid)))
            ((equal subtask-label 'Get-Wood) (list 'tabular 'Get-Wood (get-wood-tabular-features s a tid)))
            ((equal subtask-label 'Deposit) (list 'tabular 'Deposit (deposit-tabular-features s a)))
	    ((equal subtask-label 'Harvest-Gold) (list 'tabular 'Harvest-Gold (harvest_gold_value s tid)))
	    ((equal subtask-label 'Harvest-Wood) (list 'tabular 'Harvest-Wood (harvest_wood_value s tid)))
	    ((equal subtask-label 'Dropoff) (list 'tabular 'Dropoff (dropoff_value s tid)))
	    ((equal subtask-label 'Goto) (list 'linear 'Goto (goto_value s tid)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMPLETION FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun root-tabular-features (s a tid)
  (list (agent-resource s tid) (requisite_gold s) (requisite_wood s) a))

(defun get-gold-tabular-features (s a tid)
  (list (agent-resource s tid) (if (= a 16) 'harvest_gold 'goto) 
	(if (= a 16) (agent_region_gold s (peasant-xy s tid)) (agent_region_gold s (region-to-coordinate a)))))

(defun get-wood-tabular-features (s a tid)
   (list(agent-resource s tid) (if (= a 16) 'harvest_wood 'goto) 
	(if (= a 16) (agent_region_wood s (peasant-xy s tid)) (agent_region_wood s (region-to-coordinate a)))))


(defun deposit-tabular-features (s a tid)
   (list (agent-resource s tid) (requisite_gold s) (requisite_wood s) 
     (if (= a 16) 'dropoff 'goto) 
     (if (= a 16) (agent_region_townhall s (peasant-xy s tid)) (agent_region_townhall s (region-to-coordinate a)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VALUE FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun harvest_gold_value (s tid)
  (agent_region_gold s (peasant-xy s tid)))

(defun harvest_wood_value (s tid)
  (agent_region_wood s (peasant-xy s tid)))

(defun dropoff_value (s tid)
  (list (agent_resource s tid) (requisite_gold s) (requisite_wood s)
    (agent_region_townhall s (peasant-xy s tid))))

(defun goto_value (s tid)
  (vector (agent_distance_x s tid) (agent_distance_y s tid) 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FEATURE ACCESSORS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun requisite_gold (s)
  (if (<= *target-gold* (global-gold (strat-es-global (js-env-state s))))
    1
    0))

(defun requisite_wood (s)
  (if (<= *target-wood* (global-gold (strat-es-global (js-env-state s))))
    1
    0))

(defun peasant-xy (s tid)
  (let ((peasant-state (get-unit-state (js-env-state s) (cdr tid))))
      (cons (unit-x peasant-state) (unit-y peasant-state))))

;; NOTE: does not check whether a unit is attacking, repairing, etc.; this means that
;; the status args can only represent what the unit is carrying; probably have to change
;; this if peasant is allowed to attack

(defun agent_resource (s tid)
  (let ((peasant-state (get-unit-state (js-env-state s) (cdr tid))))
    (if (equalp (unit-status-args peasant-state) nil)
        'nothing
        (if (equalp (unit-status-args peasant-state) '(1))
            'gold
            (if (equalp (unit-status-args peasant-state) '(2))
                'wood)))))

;; checks if the 9x9 region around the agent coord provided as (x . y) has a unit of 
;; type ut

(defun agent_region_unit (s coord ut)
  (let ((units-of-type (get-units-by-type 0 ut (js-env-state s)))
	(peasant-x (car coord))
	(peasant-y (cdr coord))
        (in-region? 0))
    (dolist (i units-of-type)
      (let ((ux (unit-x (get-unit-state (js-env-state s) i)))
	    (uy (unit-y (get-unit-state (js-env-state s) i))))
	(if (and (<= ux (+ peasant-x 4)) (>= ux (- peasant-x 4))
		 (<= uy (+ peasant-y 4)) (>= uy (- peasant-y 4)))
	  (setf in-region? 1))))
    in-region?))

(defun agent_region_gold (s coord)
  (agent_region_unit s coord 92))

(defun agent_region_townhall (s coord)
  (agent_region_unit s coord 74))

(defun agent_region_wood (s coord)
  (let ((peasant-x (car coord))
	(peasant-y (cdr coord))
	(in-region? 0))
    (loop for x from (- peasant-x 4) to (+ peasant-x 4) until (= in-region? 1) do
      (loop for y from (- peasant-y 4) to (+ peasant-y 4) until (= in-region? 1) do
        (if (equalp (map-ref x y (global-state-map (global-state (js-env-state s))) #\T)) ;; is tree
	    (setf in-region? 1))))
    in-region?))

;; for agent_diff_x and agent_diff_y, we assume that goto(k)'s choice point 
;; (ie where this is called) has the destination loc in the choice region

(defun agent_distance_x (s tid)
  (let* ((peasant-state (get-unit-state (js-env-state s) (cdr tid)))
	(subtask-label (js-thread-label s tid))
        (dest-loc (region-to-coordinate (cdr subtask-label))))
    (abs (- (unit-x peasant-state) (car dest-loc)))))

(defun agent_distance_y (s tid)
  (let* ((peasant-state (get-unit-state (js-env-state s) (cdr tid)))
	(subtask-label (js-thread-label s tid))
        (dest-loc (region-to-coordinate (cdr subtask-label))))
    (abs (- (unit-y peasant-state) (cdr dest-loc)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UTIL FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; references location (x, y) for two-dimensional map m (represented as list of letters)

(defun map-ref (x y m)
  (if (or (< x 0) (< y 0) (> x 31) (> y 31))
    0
    (char (symbol-name (car (nth y m))) x)))

(defun coordinate-to-region (x y)
  (if (and (< x 27) (< y 27))
    (+ (floor (/ x 9)) (* 3 (floor (/ y 9))))
    (if (< x 27)
      (+ (floor (/ x 9)) 9)
      (if (< y 27)
        (+ 12 (floor (/ y 9)))
        15))))

(defun unit-x (u)
  (car (unit-loc u)))

(defun unit-y (u)
  (cadr (unit-loc u)))

