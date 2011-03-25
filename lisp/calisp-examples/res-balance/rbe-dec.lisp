(defpackage rbe-decomposed-crl-q
  (:documentation "rbe-decomposed-crl-q (rbe-dec).  Decomposed crl features for resource balance program.")
  (:use
   cl
   rbe-prog
   calisp-features
   calisp-q-function
   rbe
   set
   utils)
  (:nicknames rbe-dec)
  (:export
   make-rbe-dec-q-function))

(in-package rbe-dec)

(defun make-rbe-dec-q-function (max-gold max-wood num-peasants)
  (make-instance '<threadwise-crl-q-function>
    :feature-ids #(gold wood gold-peas wood-peas collision distance carrying? no-goal root-thread peasant-thread)
    :decomposed-template-descs
    (list
     (make-gold-fts '(root-thread) max-gold)
     (make-wood-fts '(root-thread) max-wood)
     (make-gold-fts num-peasants max-gold)
     (make-wood-fts num-peasants max-wood)
     #'collision-ft
     #'distance-ft
     #'carrying-resource?
     #'no-nav-goal
     #'root-thread
     #'peasant-thread
     )))


(defun root-thread (omega i)
  (declare (ignore omega))
  (indicator (eq i 'root-thread)))

(defun peasant-thread (omega i)
  (declare (ignore omega))
  (indicator (not (eq i 'root-thread))))

(def-decomposed-feature-template spawning-peasant-threads (omega i)
  "indicator of whether program is still at initial stage of spawning peasant threads."
  (when (eq i 'root-thread)
    (indicator 
     (hash-table-has-key thread-states 'root-thread))))


(defun make-gold-fts (s mg)
  (bin-values
   (make-decomposed-feature-template 
    (omega i)
    (when (member? i s)
      (let ((gold (gold env-state))
	    (num-getting (length (threads-in-subtask gather-gold))))
	(make-calisp-feature 
	 (choosing-threads-at-label task-choice)
	 (lambda (&rest choices)
	   (+ num-getting gold (count 'gather-gold choices)))))))
   (append (below (+ 2 mg)) '(infty))))

(defun make-wood-fts (s mw)
  (bin-values
   (make-decomposed-feature-template 
    (omega i)
    (when (member? i s)
      (let ((wood (wood env-state))
	    (num-getting (length (threads-in-subtask gather-wood))))
	(make-calisp-feature 
	 (choosing-threads-at-label task-choice)
	 (lambda (&rest choices)
	   (+ num-getting wood (count 'gather-wood choices)))))))
   (append (below (+ 2 mw)) '(infty))))


(def-decomposed-feature-template distance-ft (omega id)

  (let ((np (num-peas env-state))
	(positions (pos env-state)))
    
    ;; have one distance feature per peasant thread
    (when (member? id np)
      
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
	  0)))))

(def-decomposed-feature-template collision-ft (omega i)
  (let ((positions (pos env-state))
	(wm (world-map env-state))
	(np (num-peas env-state)))
    
    (when (member? i np)

      (labels ((at-nav-choice? (k)
		 (let ((ts (gethash k thread-states)))
		   (and ts
			(choosing ts)
			(at-label ts 'nav-choice))))
	     
	       (make-collision-feature (j)
		 (let ((pos-i (aref positions i)) (pos-j (aref positions j)))
		   (cond 
	      	      
		    ;; 1. If they're both at a nav-choice, 
		    ;; the feature depends on both of them
		    ((and (at-nav-choice? i) (at-nav-choice? j))
		     (make-calisp-feature 
		      (list i j)
		      (lambda (u v) (indicator (equal (result-legal wm pos-i u) (result-legal wm pos-j v))))))
	   
		    ;; 2. if only one of them is at a nav-choice, the feature only depends on that one
		    ((at-nav-choice? i)
		     (make-calisp-feature i (lambda (u) (indicator (equal (result-legal wm pos-i u) pos-j)))))
		    ((at-nav-choice? j)
		     (make-calisp-feature j (lambda (u) (indicator (equal pos-i (result-legal wm pos-j u))))))
	   
		    ;; 3. otherwise, just check if they're at the same place
		    (t (indicator (equal pos-i pos-j))))))
	     
	       (is-potential-collider (j)
		 (and (/= i j) 
		      (<= (shortest-path-dist wm (aref positions i) (aref positions j)) 2))))
      
	(mapset 'list #'make-collision-feature (filter 'list np #'is-potential-collider))))))


(def-decomposed-feature-template carrying-resource? (omega i)
  (when (numberp i)
    (indicator
     (member (aref (status env-state) i) '(carrying-wood carrying-gold)))))


(def-decomposed-feature-template no-nav-goal (omega i)
  (when (numberp i)
    (let ((ts (gethash i thread-states)))
      (indicator (or (not ts) (at-label ts 'task-choice) (at-label ts 'new-peasant))))))