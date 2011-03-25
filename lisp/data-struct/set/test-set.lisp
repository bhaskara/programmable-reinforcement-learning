(defpackage test-set
  (:use
   cl
   utils
   set
   prod-set))

(in-package test-set)


(setf s1 (make-instance '<var-set> :sets (list '(foo bar) 3) :element-type 'list))
(setf s2 (make-instance '<var-set> :sets (list '(foo bar) 3) :element-type 'list :iterate-quickly t))


(defun foo (s)
  (let ((iter (iterator s)))
    (list
     (funcall iter)
     (funcall iter))))

(defun bar (s)
  (let ((iter (iterator s)))
    (flet ((adder (n) (lambda (x) (+ x (second n)))))
      (let ((a (adder (funcall iter))))
	(loop repeat 4 do (funcall iter))
	(let ((b (adder (funcall iter))))
	  (list
	   (funcall a 10)
	   (funcall b 10)))))))


(do-tests
    "direct product sets"
  
  (foo s1)
  '((foo 0) (foo 1))
  
  (foo s2)
  '((foo 1) (foo 1))
  
  (bar s1)
  '(10 12)
  
  (bar s2)
  '(12 12)

  (list (size s1) (size s2))
  '(6 6))
  
  
  
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; subspaces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  

(setf doms 
  (list #(a b c) 4 6 '(foo bar) (make-instance '<var-set> :sets '((baz qux) 2)
						:iterate-quickly nil :element-type 'list)))

(setf s (make-instance '<var-set> :sets doms :iterate-quickly nil))

(setf sub (make-subspace s '(1 3 0)))

(setf iter (iterator sub))

(do-tests
    "direct product subspaces"
  (item 0 sub) #(a 0 not-used foo not-used)
  (item 10 sub) #(b 1 not-used bar not-used)
  (size sub) 24
  (member? #(c 3 not-used bar not-used) sub) t
  (member? #(c 3 nt-used bar nil) sub) nil
  (member? #(c 3 not-used ba not-used) sub) nil
  (item-number #(c 3 not-used bar not-used) sub) 23
  (funcall iter) #(a 0 not-used foo not-used)
  (loop repeat 6 do (funcall iter) finally (return (funcall iter)))
  #(b 1 not-used foo not-used))