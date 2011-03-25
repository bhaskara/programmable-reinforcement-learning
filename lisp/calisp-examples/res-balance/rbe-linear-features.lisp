(defpackage rbe-linear-features
  (:documentation "Linear features and Q-function for calisp resource balance program")
  (:use
   cl
   rbe-prog
   calisp-features
   calisp-q-function
   rbe
   set
   utils)
  (:export
   make-rbe-crl-q-function))

(in-package rbe-linear-features)


(defun make-rbe-crl-q-function (max-gold max-wood)
  "make-rbe-crl-q-function MAX-GOLD MAX-WOOD.  Return an object of type <calisp-crl-q-function>

The feature templates
- binned value of gold amount
- binned value of wood amount
- Pairwise collision features for peasants within distance 2 of each other
- Indicators for carrying resources
- Distance feature for each peasant
- Feature for peasants without a nav goal
- Constant"
  
  (flet ((make-bins (x)
	   (append
	    (loop for i upto x
		collect i)
	    '(infty))))
    (make-instance '<calisp-crl-q-function>
      :template-descs
      (list
       (bin-values #'expected-gold-amount (make-bins (1+ max-gold)))
       (bin-values #'expected-wood-amount (make-bins (1+ max-wood)))
       #'collision-ft
       #'carrying-resource?
       #'distance-ft
       #'no-nav-goal
       #'spawning-peasant-threads
       1)
      :feature-ids #(gold wood collision carrying distance no-goal spawning constant))))



(def-feature-template spawning-peasant-threads (omega)
  "indicator of whether program is still at initial stage of spawning peasant threads."
  (indicator 
   (hash-table-has-key thread-states 'root-thread)))

(def-feature-template no-nav-goal (omega)
  "Number of peasants without a navigation goal"
  (loop 
      for i below (num-peas env-state)
      for ts = (gethash i thread-states)
      count (or (not ts) (at-label ts 'task-choice) (at-label ts 'new-peasant))))
	    


(def-feature-template collision-ft (omega)
    (let ((positions (pos env-state))
	  (wm (world-map env-state))
	  (np (num-peas env-state)))
      (flet ((at-nav-choice (i)
	       (let ((ts (gethash i thread-states)))
		 (and ts
		      (choosing ts)
		      (at-label ts 'nav-choice)))))

	(mapset 
	 'list

	 ;; A. Function that maps a pair I,J of peasant ids to a collision feature
	 (lambda (x &aux (i (first x)) (j (second x)))
	   (let ((pos-i (aref positions i)) (pos-j (aref positions j)))
	     (cond 
	      ;; 1. If they're both at a nav-choice, 
	      ;; the feature depends on both of them
	      ((and (at-nav-choice i) (at-nav-choice j))
	       (make-calisp-feature 
		(list i j)
		(lambda (u v) (indicator (equal (result-legal wm pos-i u) (result-legal wm pos-j v))))))
	   
	      ;; 2. if only one of them is at a nav-choice, the feature only depends on that one
	      ((at-nav-choice i)
	       (make-calisp-feature i (lambda (u) (indicator (equal (result-legal wm pos-i u) pos-j)))))
	      ((at-nav-choice j)
	       (make-calisp-feature j (lambda (u) (indicator (equal pos-i (result-legal wm pos-j u))))))
	   
	      ;; 3. otherwise, just check if they're at the same place
	      (t (indicator (equal pos-i pos-j))))))
      
	 ;; B. the set of pairs of peasants we need collision features for
	 (filter 'list
		 ;; first, create set of all pairs of peasants
		 (make-instance 'prod-set:<var-set> :sets (list np np) :element-type 'list)

		 ;; then, pick out pairs of peasants that are close to each other
		 (lambda (x &aux (i (first x)) (j (second x)))
		   (and (< i j) ;; don't pick same pair twice
			(<= (shortest-path-dist wm (aref positions i) (aref positions j)) 2))))))))


(def-feature-template distance-ft (omega)

  (let ((np (num-peas env-state))
	(positions (pos env-state)))
    ;; have one distance feature per thread
    (mapset
     'list

     ;; A. function that takes in peasant id and produces distance feature
     #'(lambda (id)
	 (let ((pos (aref positions id))
	       (ts (gethash id thread-states))
	       (wm (world-map env-state)))
	   (if ts
	       
	       ;; if this thread exists
	       (let ((dest (stack-var-val ts 'loc))
		     (choosing? (choosing ts))
		     (returning-to-base? (some-frame-at-label ts 'dropoff-resource))
		     (map-size (apply #'+ (dimensions wm))))
		 (flet ((scaled-dist (l1 l2)
			  (/ (shortest-path-dist wm l1 l2) map-size)))
		   (cond
		    ;; 1. if making nav-choice, distance after this move, assuming it succeeds
		    ((and choosing? (at-label ts 'nav-choice))
		     (make-calisp-feature id (lambda (u) (scaled-dist dest (result-legal wm pos u)))))
	 
		    ;; 2. if choosing location to get resource from, distance after this choice
		    ((and choosing? (at-label ts 'get-resource))
		     (make-calisp-feature id (lambda (u) (scaled-dist u pos))))
	      
		    ;; 3. if returning to base
		    (returning-to-base?
		     (scaled-dist pos (rbe:base-loc env-state)))
	 
		    ;; 4. otherwise, if we have a destination, current distance to it
		    (dest (scaled-dist dest pos))
	 
		    ;; 5. otherwise, 0
		    (t 0)
		    )))
	   
	     ;; if not, 0
	     0)))
    
     ;; B. Set of peasant ids
     np)))


(def-feature-template expected-gold-amount (omega)
    ;; won't work when number of peasants is large
    (let ((gold (gold env-state))
	  (num-getting (length (threads-in-subtask gather-gold))))
      (make-calisp-feature 
       (choosing-threads-at-label task-choice)
       (lambda (&rest choices)
	 (+ num-getting gold (count 'gather-gold choices))))))

(def-feature-template expected-wood-amount (omega)
    (let ((wood (wood env-state))
	  (num-getting (length (threads-in-subtask gather-wood))))
      (make-calisp-feature
       (choosing-threads-at-label task-choice)
       (lambda (&rest choices)
	 (+ num-getting wood (count 'gather-wood choices))))))

(def-feature-template carrying-resource? (omega)
    (count-if (lambda (x) (member x '(carrying-wood carrying-gold)))
	      (status env-state)))