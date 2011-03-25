(in-package asdf)



(asdf:defsystem "hrl"
    :components ((:module "misc" :pathname "misc/" :serial t
			  :components
			  ((:file "package-utils")
			   (:file "util-pkg")
			   (:file "macros")
			   (:file "clone")
			   (:file "same")
			   (:file "canonicalize" :depends-on ("clone"))
			   (:file "hash-table-array" :depends-on ("canonicalize"))
			   
			   (:file "help")
			   (:file "utils" :depends-on ("clone"))
			   (:file "array-utils" :depends-on ("utils" "macros"))
			   (:file "list-utils" :depends-on ("utils"))
			   (:file "function-utils")
			   (:file "exp-utils")
			   (:file "matlab" :depends-on ("utils"))
			   (:file "sequence-utils" :depends-on ("utils"))
			   (:file "string-utils" :depends-on ("sequence-utils"))
			   
			   #+concurrent-alisp
			   (:file "socket-utils" :depends-on ("utils"))
			   
			   #+concurrent-alisp
			   (:file "threads" :depends-on ("utils"))))
		 
		 (:module "set" :pathname "data-struct/set/"
			  :depends-on ("misc")
			  :components ((:file "set")
				       (:file "seq-set" :depends-on ("set"))
				       (:file "number-set" :depends-on ("set"))
				       (:file "indexed-set" :depends-on ("set"))
				       (:file "named-sets" :depends-on ("set"))
				       (:file "inst-var-accessors" :depends-on ("set"))
				       (:file "create-sets" :depends-on ("set"))
				       (:file "directory-set" :depends-on ("set"))
				       (:file "direct-product-set" :depends-on ("set"))
				       (:file "var-set" :depends-on ("direct-product-set"))))
		 

		 (:module "data-struct" :pathname "data-struct/"
			  :depends-on ("misc" "prob" "set")
			  :components
			  ((:file "bucketed-counts")
			   (:file "mapping")
			   (:file "circular-vector")
			   (:module "bnet"
				    :components ((:file "bnet-pkg")
						 (:file "bnet-utils" :depends-on ("bnet-pkg"))
						 (:file "2tbn")))
			   (:module "tree"
				    :components ((:file "tree")))
			   
			   (:module "prop-logic"
				    :components ((:file "prop-formula")
						 (:file "dnf" :depends-on ("prop-formula"))
						 (:file "dnf-set" :depends-on ("dnf"))))
			   
 			   (:module "pot-set"
 				    :components ((:file "pot-pkg")
						 (:file "operation" :depends-on ("pot-pkg"))
 						 (:file "potential" :depends-on ("pot-pkg"))
 						 (:file "tabular-potential" :depends-on ("potential"))
 						 (:file "function-potential" :depends-on ("potential"))
						 (:file "number-potential" :depends-on ("potential"))
					 	 (:file "pot-set" :depends-on ("potential" "operation"))
						 (:file "undirected-graphical-model" :depends-on ("pot-set"))
						 ))

			   (:file "cache")
			   (:file "condition-variable")))
		 
		 
		 (:module "env" :depends-on ("misc")
			  :components ((:file "env-pkg")
				       (:file "env" :depends-on ("env-pkg"))
				       (:file "fully-observable-env" :depends-on ("env"))
				       ))
		 
		 (:module "fn-approx" :pathname "fn-approx/"
			  :depends-on ("misc")
			  :components ((:file "fn-approx")
				       (:file "tabular-fn-approx")
				       (:file "linear-fn-approx")
				       (:file "linear-fn-approx-with-bounds")))
		 
		 (:module "rl-functions"
			  :depends-on ("fn-approx" "misc" "data-struct" "prob" "math" "mdp")
			  :components ((:module "value-function"
						:components ((:file "value-function")
							     (:file "tabular-value-function" :depends-on
								    ("value-function"))))
				       
						
				       (:module "q-function"
						:depends-on ("value-function" "policy")
						:components ((:file "q-function")
 							     (:module "crl" :depends-on ("q-function")
 								      :components
 								      ((:file "crl-q-function")
 								       (:file "crl-features"
 									      :depends-on 
									      ("crl-q-function"))))
							     (:file "sum-q-function" 
								    :depends-on ("q-function"))
							     (:file "approx-q-function" :depends-on 
								    ("q-function"))
							     (:file "tabular-q-function" :depends-on
								    ("q-function"))
							     (:module "decomposed" :depends-on ("crl")
								      :components
								      ((:file "decomposed-q-function")
								       (:file "decomposed-crl-q-function")
								       (:file "decomposed-tabular-q-function")))
							     (:file "env-q-function" :depends-on 
								    ("approx-q-function"))
							     ))
				       
				       (:module "policy"
						:components ((:file "policy")
							     
							     (:file "random-policy" :depends-on ("policy"))
							     (:file "tabular-policy" :depends-on ("policy"))
							     (:file "prompt-policy" :depends-on ("policy"))
							     ))
				       
				       (:module "policy2" :depends-on ("q-function" "policy")
						:pathname "policy/"
						:components ((:file "greedy-policy")
							     (:module "exp-pol"
								      :components
								      ((:file "exploration-policy")))))

				       (:module "learning-rate"
						:components ((:file "learning-rate")
							     (:file "polynomial-learning-rate" 
								    :depends-on ("learning-rate"))))))
		 
		 (:module "math" :pathname "math/" :depends-on ("misc")
			  :components ((:file "lin-alg")
				       (:file "chi-square")))
		 
		 (:module "prob" :in-order-to ((compile-op (load-op "misc")))
			  :depends-on ("set")
			  :components ((:file "probability-distribution")
				       (:file "function-random-variable" :depends-on ("probability-distribution"))
				       (:file "create-distributions" :depends-on ("probability-distribution"))
				       (:file "vector-probability-distribution" :depends-on ("probability-distribution"))
				       (:file "hash-table-prob-dist" :depends-on ("probability-distribution"))
				       (:file "alist-probability-distribution" :depends-on ("probability-distribution"))))
		 
		      
		 (:module "mdp" :depends-on ("env" "data-struct" "math" "prob" "misc")
			  :in-order-to ((compile-op (load-op "misc")))						   
			  :components ((:file "mdp-pkg")
				       (:file "smdp"
					      :depends-on ("mdp-pkg"))
				       (:file "mdp"
					      :depends-on ("smdp"))
				       (:file "mdp-env"
					      :depends-on ("mdp"))
				       (:file "2tbn-mdp-env"
					      :depends-on ("mdp-env"))
				       (:file "tabular-smdp"
					      :depends-on ("smdp"))
				       (:file "hierarchical-smdp"
					      :depends-on ("smdp" "tabular-smdp"))
				       (:file "tabular-mdp"
					      :depends-on ("mdp"))))
     
		 (:module "dp" :depends-on ("mdp" "data-struct" "misc" "math")
			  :components ((:file "dp")
				       (:file "mdp-dp" :depends-on ("dp"))
				       (:file "sparse-dp" :depends-on ("dp"))
				       (:file "hierarchical-dp" :depends-on ("dp" "sparse-dp"))
				       (:file "markov-chain" :depends-on ("dp"))
				       ))
     
		 (:module "rl" :depends-on ("mdp" "data-struct" "misc" "env" "rl-functions" "dp")
			  :components ((:file "reinforcement-learning")
				       (:file "rl-observer" :depends-on ("reinforcement-learning"))
				       (:file "rl-control" :depends-on ("rl-observer" "reinforcement-learning"))
				       (:file "rl-user" :depends-on ("reinforcement-learning" "rl-observer" 
											      "rl-control" "obs"))
				       (:module "obs"
						:depends-on ("rl-observer")
						:components ((:file "progress-printer")
							     (:file "stat-gatherer")
							     (:file "env-observer")))
				       (:module "learn"
						:depends-on ("obs")
						:components ((:file "learning-algorithm")
							     (:file "q-learning" :depends-on ("learning-algorithm"))
							     (:file "approximate-policy-iteration"
								    :depends-on ("learning-algorithm"))
							     (:file "gold-standard" :depends-on ("learning-algorithm"))
							     ))))
		 
		 (:module "boltzmann-exploration" :depends-on ("rl" "rl-functions" "prob")
			  :pathname "rl-functions/policy/exp-pol/"
			  :components ((:file "boltzmann-exp-pol")
				       (:file "epsilon-boltzmann-exp-pol" :depends-on ("boltzmann-exp-pol"))))


		 
		 (:module "envs" :depends-on ("misc" "data-struct" "mdp" "math")
			  :components ((:file "grid-world")
				       (:file "maze-mdp" :depends-on ("grid-world"))
				       (:file "variable-effector-env")
				       (:module "res-balance" :depends-on ("grid-world")
						:components ((:file "rbe-2tbn")))
				       (:module "taxi"
						:depends-on ("grid-world")
						:components ((:file "td-taxi-env")
							     (:file "td-taxi-examples")
							     (:file "td-taxi-flat-lfa" :depends-on ("td-taxi-env"))
							     (:file "qe-taxi")
							     (:file "qe-taxi-smdp" :depends-on ("qe-taxi"))
							     ))))
     
			    

		 (:module "alisp" :in-order-to ((compile-op (load-op "misc")))
			  :depends-on ("misc" "env" "data-struct" "rl-functions" "rl" "dp")
			  :components ((:file "alisp")
				       (:file "alisp-state" :depends-on ("alisp"))
				       (:file "alisp-observer" :depends-on ("alisp"))
				       (:file "alisp-program" :depends-on ("alisp"))
				       (:file "rlm" :depends-on ("alisp" "alisp-observer" "alisp-program" "alisp-state"))
				       (:file "alisp-user" :depends-on ("alisp" "rlm" "alisp-program" 
										"obs" "alisp-observer"))
				       
				       (:module "rl-functions" :depends-on ("alisp" "alisp-state")
						:components ((:file "alisp-approx-q-function")
							     (:file "alisp-features")
							     (:file "exit-distribution")
							     (:file "array-exit-distribution"
								    :depends-on ("exit-distribution"))
							     (:file "holy-q-fn" :depends-on ("exit-distribution"))))
					      
				       (:module "obs"
						:depends-on ("alisp" "alisp-observer")
						:components ((:file "alisp-io-int-observer")
							     (:file "progress-printer")))
				       
				       (:module "learn"
						:depends-on ("alisp" "alisp-observer" "rl-functions")
						:components ((:file "learning-algorithm")
							     (:file "gold-standard" :depends-on ("learning-algorithm"))
							     (:file "hordq" :depends-on ("learning-algorithm"))
							     (:file "holyq" :depends-on ("hordq"))
							     (:file "rordq" :depends-on ("hordq"))
							     (:file "smdpq" :depends-on ("learning-algorithm"))))
				       ))
		 

 		 
		 #+concurrent-alisp
 		 (:module "stratagus-env" :depends-on ("misc" "data-struct" "mdp")
 			  :pathname "envs/stratagus/" :serial t
 			  :components ((:file "gameinfo")
				       (:file "stratagus-env-pkg")
 				       (:file "strat-constants")
 				       (:file "strat-env-state")
 				       (:file "stratagus-env")

				       (:module "strat-env1" :pathname "env1/" 
						:components ((:file "strat-env1")))
				       (:module "tactical-fc" :pathname "tactical-fc/"
						:components ((:file "tactical-fc-env")))))
		 
		 
		 (:module "alisp-examples" :depends-on ("env" "envs" "alisp" "misc")
			  :in-order-to ((compile-op (load-op "alisp")))
			  :components
			  ((:module "taxi" :components ((:file "td-taxi-prog")
							(:file "td-taxi-prog-features" 
							       :depends-on ("td-taxi-prog"))
							(:file "qe-taxi-prog")))))


		 #+concurrent-alisp
		 (:module "concurrent-alisp" :in-order-to ((compile-op (load-op "misc")))
 			  :depends-on ("misc" "env" "math" "data-struct" "rl-functions")
 			  :components ((:file "calisp")
 				       (:file "calisp-observer" :depends-on ("calisp"))
				       (:module "rl-functions" :depends-on ("calisp")
						:components ((:file "q-function")
							     (:file "calisp-features")
							     (:file "crl-q-function" :depends-on ("calisp-features"))
							     ))
				       (:module "obs"
						:depends-on ("calisp" "calisp-observer")
						:components ((:file "calisp-io-int-observer")
							     (:file "message-logger")
							     (:file "thread-debugger")))
				       (:module "learn"
						:depends-on ("rl-functions" "obs")
						:components ((:file "smdpq")
							     (:module "decomposed"
								      :components
								      ((:file "threadwise-decomposed-q")
								       (:file "temporally-decomposed-q")
								       (:file "reward-decomposition-debugger")))))
 				       (:file "calisp-state" :depends-on ("calisp"))
 				       (:file "calisp-program" :depends-on ("calisp" "calisp-state"))
 				       (:file "crlm" :depends-on ("calisp-program"))
				       (:file "calisp-user" :depends-on ("crlm" "learn"))))
		 
		 #+concurrent-alisp
		 (:module "calisp-examples" :in-order-to ((compile-op (load-op "concurrent-alisp")))
			  :depends-on ("misc" "env" "envs" "concurrent-alisp")
			  :components
			  ((:module "res-balance" :components ((:file "rbe-prog")
							       (:file "rbe-progs")
							       (:file "rbe-tabular-features"
								      :depends-on ("rbe-prog"))
							       (:file "rbe-linear-features"
								      :depends-on ("rbe-prog"))
							       (:file "rbe-dec"
								      :depends-on ("rbe-prog"))))
			   (:module "ve-env" :components ((:file "ve-prog1")))))
		 
		 

			   


		
		 (:module "test" :depends-on ("envs" "misc")
			  :components ((:file "mdp-test-envs")))))
	
				       






     
     

(in-package cl-user)




;; Local variables:
;; mode:lisp
;; outline-regexp:"\\s-*..module"
;; End:
