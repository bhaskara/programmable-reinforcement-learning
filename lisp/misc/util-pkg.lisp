
(defpackage utils
  (:use common-lisp)
  (:export consec
	   clone
	   undefmethod
	   
	   def-struct-clone-method
	   copy-into
	   same
	   canonicalize
	   def-struct-canonicalize-method
	   gendefstruct
	   copy
	   leave-out
	   set-if-unbound
	   as-is
	   eql-array
	   get-lambda-list
	   indicator
	   xor
	   nxor
	   below
	   eql-hash-table
	   hash-table-has-key
	   hash-keys
	   copy-hash-table
	   round-decimal
	   build-symbol-name
	   intern-compound-symbol
	   round-array
	   reshape-array
	   map-array
	   subarray
	   a+
	   a*
	   a-
	   1s
	   0s
	   array-size-multipliers
	   rand-fn-hist
	   check-exact-class
	   hash-table-select
	   classify-bin
	   alist-to-hash
	   print-hash-table-readably
	   read-object-from-file
	   sparsify
	   number-sequence
	   hash-to-alist
	   pprint-hash
	   debug-print
	   *debug-level*
	   wait-until
	   length-exceeds
	   insert-sorted-list
	   insert-at-position
	   designated-list
	   p2alist
	   is-sorted
	   force-format
	   eval-now
	   _f
	   avg
	   avgf
	   adjustf
	   multf
	   orf
	   deletef
	   adjoinf
	   maxf
	   with-gensyms
	   while
	   until
	   condlet
	   aif
	   awhen
	   awhile
	   aand
	   
	   mvbind
	   dbind
	   mvsetq
	   unbind-slot
	   
	   do-tests
	   do-rand-tests
	   do-boolean-tests
	   *rhs*
	   *lhs*
	   
	   
	   do-iterator
	   map-iterator-to-list
	   it
	   
	   to-boolean
	   bit-true
	   bit-false
	   
	   
	   abs-diff
	   between
	   between2
	   make-evenly-spaced-intervals
	   stail
	   slast
	   sfirst
	   ssecond
	   sthird
	   sfourth
	   fill-format
	   fill-format-nl
	   with-outfile
	   prompt
	   avg-over-trials
	   fn
	   
	   infty
	   -infty
	   my<
	   my<=
	   my>
	   my>=
	   mymax
	   mymin
	   argmax
	   argmin
	   
	   check-not-null
	   verify-type
	   symbol<
	   pprint-tabulated-pair
	   without-quotes
	   create-mixin
	   
	   matlab-write
	   matlab-read
	   convert-to-matlab
	   convert-from-matlab
	   
	   split-string)
  (:export "HELP"
	   "HELP-INT"))


(defpackage "HELP"
  (:use cl
	package-utils))

(in-package "HELP")
(export-from 'utils
	     "HELP"
	     "HELP-INT")
 
  

  
	   
(in-package cl-user)