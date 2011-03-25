;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; socket utils
;;
;; Utilities for dealing with sockets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Package info
(defpackage socket-utils
  (:use common-lisp
	socket)
  (:export clear-all-input))

(in-package socket-utils)


;; clear-all-input
;;
;; repeatedly call clear-input on socket s until listen returns false
;; print a "." on each attempt.  setting quiet to true suppresses this.
(defun clear-all-input (s &optional (quiet nil))
  (loop
      while (listen s)
      do (clear-input s)
	 (unless quiet (format t "."))))

(in-package cl-user)