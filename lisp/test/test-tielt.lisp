(defpackage test-tielt
  (:use common-lisp
	td-taxi-prog
	td-taxi-mdp
	rlm
	utils
	wrap-env
	tielt
	mp))

(in-package test-tielt)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; taxi mdp with tielt interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(mp:process-run-function "tielt proc" #'tielt:tielt-to-env e tielt-port)

(defparameter pass-source '((1 0)))
(defparameter pass-dest '((1 2) (1 1)))
(defparameter wm #2A((0 0 0) (0 0 0)))

;(setf dsock (socket:make-socket :connect :passive :local-host "127.1" :local-port 8111))
;(setf gsock (socket:make-socket :connect :passive :local-host "127.1" :local-port 3000))

(setf wrap-port 8765)
(setf tielt-port 4437)

(print "params defined")

(defparameter m (make-instance '<td-taxi-mdp> :wm wm :ps pass-source :pd pass-dest :msp 1))
(print "td")
(defparameter e (make-instance 'mdp-env:<mdp-env> :mdp m))
(mp:process-run-function "tielt proc" #'tielt:tielt-to-env e tielt-port)
(print "mdp")
;(setf dconn (socket:accept-connection dsock))
;(print "dconn accepted")
(defparameter wrap (make-instance 'wrap-env:<wrap-env> :local-port wrap-port))

(print "objects made")

(setf r (make-instance '<rlm> :env wrap  :root-fn #'taxi-root))
(setf epol (make-instance 'boltzmann-expl-policy:<boltzmann-expl-policy> :temperature-fn (lambda (count) (* 10 (expt .999 count)))))
(setf lrate (make-instance 'poly-lrate:<polynomial-learning-rate> :a 10 :b 0 :n 1))

(print "learning defined")

;(mp:process-run-function "tielt proc" #'tielt:tielt-to-env e tielt-port)

(print "process started")

(exec r 100)
(setf wh (learn  r 1000 lrate 1  :expl-policy epol :hist-length 100 :verbosity 1))

;(mp:process-kill (mp:process-name-to-process "tielt proc"))

;(wrap-env:close-sock wrap)



(in-package cl-user)
	
	