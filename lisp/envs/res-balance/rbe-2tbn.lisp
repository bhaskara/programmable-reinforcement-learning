(defpackage resource-balance-env
  (:documentation "Package for the resource balance environment

Creation
--------
make-rbe-2tbn
make-rbe-env

Actions
-------
N,S,E,W,P,D,R

Accessing the state
-------------------
pos
make-rbe-env-state
status
gold
wood
max-gold 
max-wood
world-map
num-peas
base-loc
forest-locs
mine-locs
collision-cost
cost-of-living
dropoff-reward

Other map functions
-------------------
result-legal
shortest-path-dist

Parameters of the environment
-----------------------------
num-peasants
grid-world
dimensions

Symbols used in state descriptions
----------------------------------
empty-handed
dropoff-gold
dropoff-wood
carrying-wood
carrying-gold
gathering
")
  (:nicknames rbe)
  (:use bnet
	cl
	prob
	create-env
	utils
	set
	prod-set
	inst-vars)
  (:import-from 
   grid-world
   N S E W
   result-legal
   rot-clockwise
   rot-counterclockwise
   dimensions
   shortest-path-dist)
		
  (:export
   make-rbe-2tbn
   make-rbe-env
   N S E W P D R
   make-rbe-env-state
   pos 
   status
   gold
   wood
   max-gold
   max-wood
   world-map
   num-peas
   base-loc
   forest-locs
   mine-locs
   collision-cost
   cost-of-living
   dropoff-reward
   
   result-legal
   shortest-path-dist
   
   num-peas 
   grid-world
   dimensions
   
   empty-handed
   dropoff-gold
   dropoff-wood
   carrying-wood
   carrying-gold
   gathering
   ))
   
   

(in-package rbe)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; state rep
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (rbe-env-state (:conc-name nil))
  "rbe-env-state - state for resource balance environment.

State variables that change during an episode
1) pos - array of positions of each peasant
2) status - array of statuses of each peasant.  Each one can be 'empty-handed, 'carrying-gold, 'carrying-wood, 'dropoff-gold, 'dropoff-wood, or '(gathering i) for some integer i, which means that the peasant will be carrying the resource i+1 steps in the future
3) gold - amount of gold at base
4) wood - amount of wood at base

State variables that don't change during an episode
5) world map - A 2d array.  For now, we only care about the dimensions of this array.
6) num-peas - number of peasants
7) max-gold
8) max-wood
9) base-loc - where the base is
10) forest-locs - where the forests are
11) mine-locs - where the gold mines are
12) cost-of-living
13) collision-cost
14) dropoff-reward"
  
  ;; TODO : should replace the env params with a single object
  
  pos 
  status
  gold
  wood
  world-map
  num-peas
  max-gold
  max-wood
  base-loc
  forest-locs
  mine-locs
  cost-of-living
  collision-cost
  dropoff-reward)

(defmethod effectors ((s rbe-env-state))
  (below (num-peas s)))


(defmethod canonicalize ((s rbe-env-state))
  (list (pos s) (status s) (gold s) (wood s)))

(defun pprint-rbe-state (str s)
  (pprint-logical-block (str nil)
    (loop
	with wm = (world-map s)
	with peas-list = (below (num-peas s))
	with num-rows = (array-dimension wm 0)
	with num-cols = (array-dimension wm 1)
	with box-size = 3
	with (row row-offset col col-offset pos)
	with positions = (pos s)
	with status = (status s)
		
	for y below (* box-size num-rows)
	do (multiple-value-setq (row row-offset)
	     (floor y box-size))
	   (loop
	       for x below (* box-size num-cols)
	       do (multiple-value-setq (col col-offset)
		    (floor x box-size))
		  (setf pos (list row col))
		  (format str
			  (or
			   (cond
			    ((or (= 0 col-offset) (= 0 row-offset)) "X")
			    ((and (= 1 col-offset) (= 1 row-offset))
			     (let ((i (find-if
				       (lambda (x) 
					 (and (equal (aref positions x) pos)
					      (member (aref status x) '(empty-handed dropoff-gold dropoff-wood))))
				       peas-list)))
			       (when i (format nil "~a" i))))
			    ((and (= 1 col-offset) (= 2 row-offset))
			     (let ((i (find-if 
				       (lambda (x)
					 (and (equal (aref positions x) pos)
					      (member (aref status x) '(carrying-gold carrying-wood))))
				       peas-list)))
			       (when i (format nil "~a" i))))
			    ((and (= 2 col-offset) (= 1 row-offset))
			     (let ((i (find-if
				       (lambda (x)
					 (and (equal (aref positions x) pos)
					      (listp (aref status x))))
				       peas-list)))
			       (when i (format nil "~a" i))))
			    ((and (= 2 col-offset) (= 2 row-offset))
			     (if (member pos (forest-locs s) :test #'equal)
				 "F"
			       (if (member pos (mine-locs s) :test #'equal)
				   "M"
				 (when (equal pos (base-loc s)) "B")))))
			   " "))
	       finally (format str "X"))
	   (pprint-newline :mandatory str)

	   
	finally (format str "~V@{~C~:*~}" (1+ (* box-size num-cols)) #\X)
		(format str "~:@_Positions : ~a~:@_Statuses : ~a~:@_Gold : ~a Wood : ~a" 
			(pos s) (status s) (gold s) (wood s))
		)))


(defmethod print-object ((s rbe-env-state) str)
  (if (or *print-readably* (not *print-pretty*))
      (call-next-method)
    (pprint-rbe-state str s)))
	   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; variable
;; descriptions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; peasant pos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
(defun make-pos-cpd (wm slip-prob)
  (make-cpd 
   (prev-pos prev-status act)
   (let ((slip-dist `((nil . ,(- 1 slip-prob)) (left . ,(/ slip-prob 2))
					       (right . ,(/ slip-prob 2)))))
     (if (or (listp prev-status) (member act '(P D R)))
	 (make-deterministic-dist prev-pos)
       (make-rv-dist (lambda (x)
		       (result-legal wm prev-pos
					(case x
					  (left (rot-counterclockwise act))
					  (right (rot-clockwise act))
					  (otherwise act))))
		     slip-dist)))))

(defun make-pos-desc (wm slip-prob i)
  (let ((positions 
	 (make-instance '<var-set> :sets (array-dimensions wm) :element-type 'list)))
    (make-2tbn-var-desc
     :id `(pos ,i) :domain positions
     :init-slice-parents () :init-dist (constantly (make-unif-dist-over-set positions))
     :prev-slice-parents `((pos ,i) (status ,i) (action ,i)) :curr-slice-parents ()
     :trans-dist (make-pos-cpd wm slip-prob))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; peasant status
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-status-desc (i gather-time-dist)
  (make-2tbn-var-desc
   :id `(status ,i) :domain nil ;; for now, because it's a pain to specify the actual set
   :init-slice-parents () :init-dist (constantly (make-deterministic-dist 'empty-handed))
   :prev-slice-parents `((pos ,i) (status ,i) (action ,i)) 
   :curr-slice-parents '(base-loc forest-locs mine-locs)
   :trans-dist (make-status-cpd gather-time-dist)))

(defun make-status-cpd (gather-time-dist)
  (make-cpd 
   (prev-pos prev-status a base-loc forest-locs mine-locs)
   
   (if (and (eq a 'P) ;; pickup
	    (eq prev-status 'empty-handed)
	    (or (member prev-pos forest-locs :test #'equal)
		(member prev-pos mine-locs :test #'equal)))
       (make-rv-dist (lambda (gt) `(gathering ,gt)) gather-time-dist)
       
     (make-deterministic-dist
      (or
       (cond
	;; if gathering a resource
	((listp prev-status)
	 (let ((time-left (second prev-status)))
	   (if (zerop time-left)
	       (if (member prev-pos forest-locs :test #'equal)
		   'carrying-wood
		 'carrying-gold)
	     `(gathering ,(1- time-left)))))
     
	;; if just dropped off something
	((member prev-status '(dropoff-gold dropoff-wood)) 'empty-handed)
    
	;; if doing a dropoff
	((and (eq a 'D) 
	      (equal prev-pos base-loc))
	 (case prev-status
	   (carrying-gold 'dropoff-gold)
	   (carrying-wood 'dropoff-wood))))
     
       prev-status ;; in default case, status doesn't change
       )))))
       
	   

    


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; resource amounts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-gold-desc (num-peas)
  (make-2tbn-var-desc :id 'gold :domain 'natural-numbers
		      :prev-slice-parents '(gold)
		      :curr-slice-parents (loop for i below num-peas collecting `(status ,i))
		      :trans-dist 
		      (make-cpd 
		       (prev-gold &rest args)
		       (make-deterministic-dist
			(+ prev-gold (count 'dropoff-gold args))))
		      :init-slice-parents '()
		      :init-dist (constantly (make-deterministic-dist 0))))

(defun make-wood-desc (num-peas)
  (make-2tbn-var-desc :id 'wood :domain 'natural-numbers
		      :prev-slice-parents '(wood)
		      :curr-slice-parents (loop for i below num-peas collecting `(status ,i))
		      :trans-dist 
		      (make-cpd (prev-wood &rest args)
				(make-deterministic-dist
				 (+ prev-wood (count 'dropoff-wood args))))
		      :init-slice-parents '()
		      :init-dist (constantly (make-deterministic-dist 0))))
				       

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dropoff rewards
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-dropoff-reward-desc (r)
  (make-2tbn-var-desc
   :id 'dropoff :domain 'natural-numbers
   :prev-slice-parents '(max-gold max-wood gold wood) :curr-slice-parents '(gold wood)
   :reward-fn
   (make-cpd
    (max-gold max-wood prev-gold prev-wood gold wood)
    (* r (+
	  (- (min max-gold gold) (min max-gold prev-gold))
	  (- (min max-wood wood) (min max-wood prev-wood)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; collision cost
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-collision-reward-desc (num-peas collision-cost)
  (make-2tbn-var-desc
   :id 'collision :domain 'natural-numbers
   :prev-slice-parents ()
   :curr-slice-parents (mapset 'list (lambda (i) `(pos ,i)) num-peas)
   :reward-fn 
   (make-cpd 
    (&rest args)
    (* (- collision-cost)
       (reduce #'+
	       (maplist 
		(lambda (positions)
		  (count (first positions) (rest positions) :test #'equal))
		args))))))


	  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	  
;; the 2tbn
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-rbe-2tbn (world-map num-peas max-gold max-wood base-loc forest-locs mine-locs
		      &key (slip-prob .1) (collision-cost 5) (cost-of-living 1)
			   (gather-time-dist '((3 . 0.5) (4 . 0.5))) (dropoff-reward 3))

  "make-rbe-2tbn  MAP NUM-PEAS MAX-GOLD MAX-WOOD BASE-LOC FOREST-LOCS MINE-LOCS &key (SLIP-PROB .1) (COLLISION-COST 5) (COST-OF-LIVING 1) (GATHER-TIME-DIST '((3 . 0.5) (4 . 0.5))) (DROPOFF-REWARD 3) 
  
Return a '<2tbn>."
  
  (make-instance '<2tbn>
    
    :state-descs 
    (list*
     (make-gold-desc num-peas)
     (make-wood-desc num-peas)
     (make-param-desc 'world-map world-map)
     (make-param-desc 'num-peas num-peas)
     (make-param-desc 'max-gold max-gold)
     (make-param-desc 'max-wood max-wood)
     (make-param-desc 'base-loc base-loc)
     (make-param-desc 'forest-locs forest-locs)
     (make-param-desc 'mine-locs mine-locs)
     (make-param-desc 'cost-of-living cost-of-living)
     (make-param-desc 'dropoff-reward dropoff-reward)
     (make-param-desc 'collision-cost collision-cost)
     (append
      (mapset 'list (lambda (i) (make-pos-desc world-map slip-prob i)) num-peas)
      (mapset 'list (lambda (i) (make-status-desc i gather-time-dist)) num-peas)))
    
    :action-descs
    (mapset 'list (lambda (i) (make-2tbn-var-desc :id `(action ,i) :domain '(N E S W R P D))) num-peas)
    
    :reward-descs
    (list (make-2tbn-var-desc :id 'cost-of-living :domain 'real-numbers
			      :prev-slice-parents () :curr-slice-parents ()
			      :reward-fn (constantly (- cost-of-living)))
	  (make-dropoff-reward-desc dropoff-reward)
	  (make-collision-reward-desc num-peas collision-cost))
    
    :state-acc (make-struct-accessors (rbe-env-state :conc-name "")
				      (gold wood world-map num-peas
					    max-gold max-wood base-loc 
					    forest-locs mine-locs
					    cost-of-living dropoff-reward
					    collision-cost 
					    (pos array num-peas) 
					    (status array num-peas)))
    :action-acc (make-alist-accessors (below num-peas))
    :reward-acc (make-vec-accessors 3)))


(defun num-peasants (e)
  "num-peasants RES-BALANCE-ENV.  Return number of peasants in a resource balance environment."
  (num-peas (get-state e)))

(defun grid-world (e)
  "grid-world RES-BALANCE-ENV.  Return the world map of a resource balance environment."
  (world-map (get-state e)))

(defun make-rbe-env (&rest args)
  "make-rbe-env  MAP NUM-PEAS MAX-GOLD MAX-WOOD BASE-LOC FOREST-LOCS MINE-LOCS &key (SLIP-PROB .1) (COLLISION-COST 5) (COST-OF-LIVING 1) (GATHER-TIME-DIST '((3 . 0.5) (4 . 0.5))) (DROPOFF-REWARD 3).  Returns an environment."
  (let ((mg (third args))
	(mw (fourth args)))
    (mdp-env:make-2tbn-mdp-env 
     (apply #'make-rbe-2tbn args)
     :term-pred #'(lambda (s) (and (>= (gold s) mg) (>= (wood s) mw))))))
