;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gameinfo.lisp
;; defines the structure that holds the parameters of the game
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage gameinfo
  (:export gameinfo
	   gameinfo-player-id
	   gameinfo-length
	   gameinfo-width
	   make-gameinfo))

(in-package gameinfo)

(defstruct gameinfo
  player-id
  length
  width)


(in-package cl-user)