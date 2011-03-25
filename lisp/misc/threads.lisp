;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; threads.lisp
;;
;; Functions for dealing with the Allegro multiprocessing system
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Package info
(defpackage threads
  (:use common-lisp
	mp
	utils)
  (:export establish-bindings
	   kill-proc
	   
	   process-reset
	   process-name
	   process-preset
	   process-enable
	   process-unlock
	   process-wait
	   make-process-lock
	   process-lock
	   process-lock-locker
	   make-process
	   process-kill
	   *current-process*
	   *all-processes*
	   with-process-lock
	   ))

(in-package threads)



;; kill-proc
;; kill all processes with given names
;;
;; names : list of names
(defmethod kill-proc ((names list))
  (loop
      for p in *all-processes*
      if (member (process-name p) names :test #'(lambda (x y) (search y x)))
      do (process-kill p)))


;; kill-proc
;; special case of the above when there's just a single process
;;
;; name : string
(defmethod kill-proc ((name string))
  (kill-proc (list name)))



;; establish-bindings
;; used to set dynamic bindings when running a new process.
;; 
;; symbol-list : list of symbols
;; value-list : list of values
;; fn : function
;; args : arguments
;;
;; See the Allegro online documentation, multiprocessing section 7 for how to 
;; use this.
(defun establish-bindings (symbol-list value-list fn args)
  (progv symbol-list value-list (apply fn args)))





(in-package cl-user)