(defpackage test-prop-logic
  (:use 
   cl
   utils
   set
   prop-logic))

(in-package test-prop-logic)


(setf f1 'foo)
(setf f2 (disjoin 'foo 'bar))
(setf f3 (conjoin 'foo (negate 'bar)))
(setf f4 (disjoin t (conjoin 'foo 'bar) (conjoin (negate 'bar) (negate 'baz))))
(setf f5 (negate 'qux))
(setf f6 (conjoin 'foo (disjoin 'bar 'baz)))


(do-boolean-tests 
    "Propositional formula types"
  (is-formula t) t
  (is-formula nil) t
  (is-formula 'foo) t
  (is-formula 3) nil
  (is-formula '(a b)) nil 
  (is-formula '(or)) t
  (is-formula '(and nil (or (and foo t) (and (not bar))) (not (and bar (or baz nil))))) t
  (is-formula '(and nil (or (and foo t) (and (not bar))) (not nil (and bar (or baz nil))))) nil
  (is-formula '(and nil (or (and (foo) t) (and (not bar))) (not nil (and bar (or baz nil))))) nil
  (is-literal f1) t
  (is-literal t) nil
  (is-literal nil) nil
  (is-literal f5) t
  (is-literal f2) nil
  (is-literal f3) nil
  (is-dnf-clause f1) t
  (is-dnf-clause f2) nil
  (is-dnf-clause f3) t
  (is-dnf-clause t) t
  (is-dnf-clause nil) t
  (is-dnf-clause f4) nil
  (is-dnf-formula t) t
  (is-dnf-formula nil) t
  (is-dnf-formula f1) t
  (is-dnf-formula f2) t
  (is-dnf-formula f3) t
  (is-dnf-formula f4) t
  (is-dnf-formula f5) t
  (is-dnf-formula f6) nil)
  
  

(let ((s1 nil)
      (s2 '(foo bar baz))
      (s3 '(baz qux foo)))
  (do-boolean-tests
      "Proposition formula holds function"
    (holds s1 t) t
    (holds s3 t) t
    (holds s1 nil) nil
    (holds s2 nil) nil
    (holds s1 'bar) nil
    (holds s2 'bar) t
    (holds s3 'bar) nil
    (holds s1 '(not qux)) t
    (holds s2 '(not qux)) t
    (holds s3 '(not qux)) nil
    (holds s2 '(or (and foo bar) (not qux))) t
    (holds s3 '(or (and foo bar) (not qux))) nil
    (holds s1 '(or (and foo bar) (not qux))) t))



(setf f1 '(or (and foo bar) nil (not baz) (and (not bar) foo)))
(setf f2 '(or (and baz qux) (and (not qux) (not foo))))

(do-tests
    "DNF operations"
  (simplify-dnf t) t
  (simplify-dnf nil) nil
  (simplify-dnf 'foo) 'foo
  (simplify-dnf '(not foo)) '(not foo)
  (simplify-dnf '(and foo bar foo)) '(and bar foo)
  (simplify-dnf '(and foo bar foo (not bar))) nil
  (simplify-dnf '(or foo bar (and foo qux (not qux)))) '(or foo bar)
  (simplify-dnf '(or foo bar (and foo qux (not qux)) t)) t
  (sort-dnf (dnf-or f1 f2))
  '(or (and bar foo) (and (not bar) foo) (and baz qux) (not baz) 
    (and (not foo) (not qux)))
  (sort-dnf (dnf-and f1 f2))
  '(or (and bar baz foo qux) (and (not bar) baz foo qux) (and (not baz) (not foo) (not qux))
    ))



(do-boolean-tests
    "DNF implication"
  (implies '(or (and foo bar) (and bar (not baz))) 'foo)
  nil
  
  (implies '(or (and foo bar) (and baz (not foo))) 'foo)
  nil
  
  (implies '(or (and foo bar) (and baz (not foo))) t)
  t
  
  (implies '(or (and foo bar) (and baz foo)) nil)
  nil
  
  (implies '(or (and foo bar) (and baz foo)) 'foo)
  t)

      

(setf s (make-dnf-set f1 '(foo bar qux baz)))
(setf s2 (make-dnf-set f2 '(foo bar qux baz)))
(setf s3 (intersect s s2))
(setf s4 (binary-union s s2))

(do-tests
    "DNF sets"
  (member? '(foo bar qux) s) t
  (member? '(baz) s) nil
  (member? '(foo bar qux) s3) nil
  (member? '(foo baz qux) s3) t
  (member? '(foo bar qux) s4) t
  (member? '(foo baz qux) s4) t
  )
