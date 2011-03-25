(in-package qe-taxi)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(gendefstruct qe-js 
	      (stack copy as-is)
	      (dest clone as-is)
	      (pass copy as-is)
	      (nav-to copy as-is)
	      (env-state copy canonicalize))


(defmacro change-fields (x fields (&rest vals))
  "change-fields X F (&rest NEW-VALUES).
X joint state.
F (not evaluated) list of field-names
NEW-VALUES list of values"
  (let ((y (gensym)))
    `(let ((,y (clone ,x)))
       ,@(mapcar #'(lambda (f v)
		     `(setf (,(intern-compound-symbol "QE-JS-" f) ,y) ,v))
		 fields vals)
       ,y)))


(defmacro equal-at-fields (x y fields)
  `(and
    ,@(mapcar (lambda (f)
		(let ((reader (intern-compound-symbol "QE-JS-" f)))
		  `(equalp (,reader ,x) (,reader ,y))))
	      fields)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; class def
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <qe-taxi-smdp> (mdp:<hierarchical-smdp>)
  ((2tbn :initarg :2tbn
	 :reader 2tbn)
   (num-pass :initarg :num-pass
	     :reader num-pass
	     :type fixnum)))

(defun make-start-state (s)
  (make-qe-js :env-state s :stack 'top
	      :dest 'no-dest :pass 'no-pass :nav-to 'no-nav))

(defun start-state-inv (omega)
  (if (and (eq (qe-js-stack omega) 'top)
	  (eq (qe-js-dest omega) 'no-dest)
	  (eq (qe-js-pass omega) 'no-pass)
	  (eq (qe-js-nav-to omega) 'no-nav))
      (qe-js-env-state omega)
    (error 'rv-inverse-not-defined :y omega)))
    
    

(defun make-key-fn (wm)
  (let ((c (array-dimension wm 1))
	(num-locs (apply #'* (array-dimensions wm))))
    (flet ((loc-num (l)
	     (+ (* (first l) c) (second l))))
      (lambda (omega)
	(let ((s (qe-js-env-state omega)))
	  (+ (* num-locs (loc-num (taxi-env-state-pos s)))
	     (loc-num (aref (taxi-env-state-src s) 0))))))))

(defun max-key (wm)
  (expt (apply #'* (array-dimensions wm)) 2))
      


(defun make-qe-taxi-smdp (2tbn n wm &optional (print-progress nil))
  "make-qe-taxi-smdp 2tbn NUM-PASS &optional (PRINT-PROGRESS nil)"
    (make-instance '<qe-taxi-smdp> :2tbn 2tbn :num-pass n
		   :compute-state-set t  :print-progress print-progress :init-set
		   (set:make-image-set (sample-space (init-dist 2tbn))
				       #'make-start-state #'qe-js-env-state)
		   :key-fn (make-key-fn wm) :max-key (max-key wm)))

(defun taxi-init-dist (m)
  (make-invertible-rv-dist #'make-start-state #'start-state-inv (init-dist (2tbn m))))
		 
					

(defmethod mdp:smdp-trans-dist ((m <qe-taxi-smdp>) omega u &aux (2tbn (2tbn m)))
  (case (qe-js-stack omega)
    (top (make-deterministic-dist (mdp:make-outcome 
				   (change-fields omega (stack) ('pass-choice)) 
				   0 0)))
    (nav-src (make-deterministic-dist 
	      (mdp:make-outcome (change-fields omega (stack dest nav-to)
					       ('nav-choice
						(aref (taxi-env-state-src (qe-js-env-state omega))
						      (qe-js-pass omega))
						'src))
				0 0)))
    (nav-dest (make-deterministic-dist
	       (mdp:make-outcome
		(change-fields omega (stack dest nav-to)
			       ('nav-choice
				(aref (taxi-env-state-dest (qe-js-env-state omega))
				      (qe-js-pass omega))
				'dest))
		0 0)))
    (get-pass (make-deterministic-dist
	       (mdp:make-outcome
		(change-fields omega (stack) ('nav-src))
		0 0)))
    (put-pass (make-deterministic-dist
	       (mdp:make-outcome
		(change-fields omega (stack) ('nav-dest)) 
		0 0)))
    (pass-choice (make-deterministic-dist
		  (mdp:make-outcome
		   (change-fields omega (stack pass) ('get-pass u)) 
		   0 0)))
    (pickup-action
     (let ((s (qe-js-env-state omega)))
       (make-invertible-rv-dist 
	(lambda (s2)
	  (mdp:make-outcome (change-fields omega (stack env-state) ('put-pass s2))
			    (reward 2tbn s 'P s2) 1))
	(lambda (outcome)
	  (let* ((omega2 (mdp:outcome-state outcome))
		 (s2 (qe-js-env-state omega2)))
	    
	    (if (and (mdp:outcome-p outcome)
		     (= (mdp:outcome-duration outcome) 1)
		     (equal-at-fields omega omega2 (dest pass nav-to))
		     (eq (qe-js-stack omega2) 'put-pass)
		     (= (mdp:outcome-reward outcome) (reward 2tbn s 'P s2)))
		s2
	      (error 'rv-inverse-not-defined))))
		     

	(prob:cond-dist 2tbn (cons s 'P)))))
    (dropoff-action
     (let ((s (qe-js-env-state omega)))
       (make-invertible-rv-dist
	(lambda (s2)
	  (mdp:make-outcome (change-fields omega (stack env-state) ('serve-exit s2))
			    (reward 2tbn s 'D s2) 1))
	(lambda (outcome &aux (omega2 (mdp:outcome-state outcome)) (s2 (qe-js-env-state omega2)))
	  (if (and (mdp:outcome-p outcome) (= (mdp:outcome-duration outcome) 1)
		   (equal-at-fields omega omega2 (dest pass nav-to))
		   (eq (qe-js-stack omega2) 'serve-exit)
		   (= (mdp:outcome-reward outcome) (reward 2tbn s 'D s2)))
	      s2
	    (error 'rv-inverse-not-defined)))
	(prob:cond-dist 2tbn (cons s 'D)))))
    (nav-choice
     (let ((s (qe-js-env-state omega)))
       (make-invertible-rv-dist
	(lambda (s2)
	  (mdp:make-outcome
	   (change-fields 
	    omega 
	    (stack env-state)
	    ((if (equal (taxi-env-state-pos s2) (qe-js-dest omega))
		 (if (eq (qe-js-nav-to omega) 'src)
		     'pickup-action
		   'dropoff-action)
	       'nav-choice)
	     s2))
	   (reward 2tbn s u s2) 
	   1))
	(lambda (outcome)
	  (let* ((omega2 (mdp:outcome-state outcome))
		 (s2 (qe-js-env-state omega2)))
	    (if (and (mdp:outcome-p outcome) (= (mdp:outcome-duration outcome) 1)
		     (equal-at-fields omega omega2 (dest pass nav-to))
		     (eq (qe-js-stack omega2)
			 (if (equal (taxi-env-state-pos s2) (qe-js-dest omega))
			     (if (eq (qe-js-nav-to omega) 'src)
				 'pickup-action
			       'dropoff-action)
			   'nav-choice))
		     (= (mdp:outcome-reward outcome) (reward 2tbn s u s2)))
		s2
	      (error 'rv-inverse-not-defined outcome))))
	(prob:cond-dist 2tbn (cons s u)))))
    (serve-exit (make-deterministic-dist 
		 (mdp:make-outcome (change-fields omega (stack) ('pass-choice-exit)) 
				   0 0)))
    (pass-choice-exit (make-deterministic-dist
		       (mdp:make-outcome (change-fields omega (stack) ('top))
					 0 0)))
    )
  )

(defmethod mdp:terminal? ((m <qe-taxi-smdp>) omega)
  (and (eq (taxi-env-state-term (qe-js-env-state omega)) 'term)
       (eq (qe-js-stack omega) 'top)))

(defmethod mdp:avail-actions ((m <qe-taxi-smdp>) omega)
  (ecase (qe-js-stack omega)
    ((top serve-exit dropoff-action pickup-action get-pass put-pass nav-src nav-dest pass-choice-exit) '(no-choice))
    (nav-choice '(N E S W))
    (pass-choice (num-pass m))))


(defmethod mdp:level ((m <qe-taxi-smdp>) omega)
  (ecase (qe-js-stack omega)
    (top 0)
    ((pass-choice pass-choice-exit) 1)
    ((get-pass put-pass serve-exit) 2)
    ((nav-src pickup-action nav-dest dropoff-action) 3)
    (nav-choice 4)))

	 