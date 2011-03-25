;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hrl-system.lisp
;;
;; Sets up the global variables for the hrl code and loads external packages necessary.  
;; In Lisp, make sure your working directory is
;; hrl/lisp (check this using the command :pwd), then type :ld hrl-system
;; After loading, use the make or make-all functions to compile and load the hrl code.
;; 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package cl-user)


;; The next line sets the compiler to produce code that is easy to debug.
;; Should be changed if speed becomes a big priority.  
(proclaim '(optimize (speed 2) (safety 1) (space 1) (debug 3)))

(load "external/asdf/asdf.lisp")

(push "." asdf:*central-registry*)


(defun make-all ()
  "make-all.  Compiles and loads all source files in hrl system."
  (asdf:operate 'asdf:load-op 'hrl :force t)
  (use-package "HELP"))




(defun make ()
  "make.  Recompiles modified source files and their dependents, and reloads binaries as necessary."
  (asdf:operate 'asdf:load-op 'hrl)
  (use-package "HELP"))
  
