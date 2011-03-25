;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list-utils.lisp
;;
;; general utilities for handling lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package utils)

(defun length-exceeds (list n)
  "length-exceeds LIST N.  Check if the length of the list exceeds N, in time proportional to min(|LIST|,N). "
  (do ((l list (cdr l))
       (m n (decf m)))
      ((or (<= m 0) (endp l))
       (not (endp l)))))
  

(defun insert-sorted-list (item l &optional (pred #'<=))
  "insert-sorted-list ITEM LIST &OPTIONAL (PRED #'<=).  Insert ITEM at the first applicable place in the sorted list LIST, where X comes before Y iff PRED(X,Y) is true.  Might modify the original list.  Return the new list."
  
  (if (when l (funcall pred (first l) item))
      (loop
	  for l1 on l
	  for l2 in (rest l)   
	  while (funcall pred l2 item)
	  finally (setf (cdr l1) (cons item (cdr l1)))
		  (return l))
    (cons item l)))



(defun insert-at-position (l x &optional pos)
  "insert-at-position LIST ITEM &optional POSITION.  Return a list consisting of LIST with ITEM added in at the given POSITION.  If POSITION is not provided, or is provided and equals nil, it defaults to (length LIST), which corresponds to adding the new item at the end.  Note that LIST may be destructively modified."
  (let ((pos (or pos (length l))))
    (if (zerop pos)
	(cons x l)
      (let ((sublist (nthcdr (1- pos) l)))
	(setf (cdr sublist) (cons x (cdr sublist)))
	l))))

(declaim (inline designated-list))
(defun designated-list (x)
  "designated-list X.  Return X if it's a list or (list X) otherwise."
  (if (listp x) x (list x)))


(defun p2alist (&rest args)
  "p2alist A1 B1 ... An Bn.  Return the association list that associates each Ai to the corresponding Bi."
  (multiple-value-bind (div rem)
      (floor (length args) 2)
    (assert (= 0 rem) () "List ~a does not have even length" args)
    (let ((alist (make-list div)))
      (do ((current alist (cdr current))
	   (plist args (cddr plist)))
	  ((null plist) alist)
	(setf (car current) (cons (car plist) (cadr plist)))))))


(in-package cl-user)

