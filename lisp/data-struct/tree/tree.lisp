(defpackage tree
  (:documentation "Package containing a simple implementation of trees in which nodes and edges can be labelled.


Node constructor and accessors
------------------------------
node
make-node
child-edges
parent-edge
node-label

Edge constructor and accessors
------------------------------
edge
make-edge
head
tail
edge-label

Consistency checking
--------------------
is-inconsistent-tree
not-root
cycle
incorrect-parent-edge
incorrect-child-edge

Basic Operations
----------------
get-root
get-child
children
get-parent
get-child-edge-by-label
get-descendant
depth
add-new-child
add-subtree
remove-subtree
remove-subtree-below
leaf?
num-nodes

Copying
-------
copy-subtree
copy-subtree-below

Iteration
---------
preorder-iterator
do-preorder
map-preorder
postorder-iterator
do-postorder
map-postorder

Debugging
---------
print-tree
*current-node*
*max-print-depth*
move-child
move-par
print-current-node
")

  (:use cl
	utils)
  (:export
   make-node
   child-edges
   parent-edge
   node-label
   node
   
   make-edge
   head
   tail
   edge-label
   edge
   
   is-inconsistent-tree
   is-consistent-tree
   cycle
   incorrect-child-edge
   incorrect-parent-edge
   not-root
   
   get-root
   get-child
   children
   get-parent
   get-child-edge-by-label
   get-descendant
   depth
   
   add-new-child
   add-subtree
   remove-subtree
   remove-subtree-below
   leaf?
   num-nodes
   
   copy-subtree
   copy-subtree-below
   
   preorder-iterator
   do-preorder
   map-preorder
   postorder-iterator
   do-postorder
   map-postorder
   
   print-tree
   *current-node*
   *max-print-depth*
   move-child
   move-par
   print-current-node
   ))

(in-package tree)
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data structures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (node (:conc-name nil))
  "Structure type node.  Create using make-node with initargs
:child-edges
:parent-edge
:node-label"
  (child-edges nil "list of child edges.")
  (parent-edge nil "either the parent edge, or null if there's no parent (root node).")
  (node-label nil "The node's label.  Nil by default."))

(defstruct (edge (:conc-name nil) (:constructor make-edge (tail head &optional edge-label)))
  "Structure edge.  Create using make-edge TAIL HEAD &optional (EDGE-LABEL)."
  (edge-label nil "Edge's label.  Nil by default.")
  (head nil "Head node of edge.")
  (tail nil "tail node of edge."))

(defvar *current-node* nil)
(defvar *max-print-depth* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; conditions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition nonexistent-child ()
  ((node :initarg :node :reader node)
   (num :initarg :num :reader num))
  (:report (lambda (c s) (format s "Node ~a does not have child #~a"
				 (node c) (num c)))))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun is-consistent-tree (node &optional (is-root t))
  "is-consistent-tree TREE &optional (IS-ROOT t) ==> boolean
Returns the negation of is-inconsistent-tree."
  (not (is-inconsistent-tree node is-root)))

(defun is-inconsistent-tree (node &optional (is-root t))
  "is-inconsistent-tree TREE &optional (IS-ROOT t) ==> generalized-boolean
Is TREE an inconsistent tree.  Checks
1. does the parent edge and every child edge of a node have that node as their tail?
2. Does the parent edge of a node have the node's parent as head
3. Are there any undirected cycles?
4. If is-root, then is TREE the root node (i.e. not having a parent?)

If the tree is consistent, returns nil.  Otherwise, returns one of 'cycle, 'not-root, 'incorrect-parent-edge, 'incorrect-child-edge"

  (let ((nodes-seen nil))

    (labels
	((make-sure-that (c problem)
	   (unless c (return-from is-inconsistent-tree problem)))
	   
	 (check-recursively (n p)
	   
	   ;; make sure node hasn't been seen already (cycle)
	   (make-sure-that (not (member n nodes-seen)) 'cycle)
	   (push n nodes-seen)
	     
	   ;; make sure parent is as expected
	   (let ((p-edge (parent-edge n)))
	     (when p
	       (make-sure-that
		(and p-edge
		     (member p-edge (child-edges p)))
		'incorrect-parent-edge
		)))
	     
	   (let ((children (child-edges n)))
	       
	     ;; make sure child edges have n as tail
	     (make-sure-that 
	      (every (lambda (e) (eq n (tail e))) children)
	      'incorrect-child-edge)
	     
	     ;; call recursively on children
	     (dolist (e (child-edges n))
		(check-recursively (head e) n)))))
    
      ;; if required, ensure that node is root, i.e. par is nil
      (when is-root (make-sure-that (not (parent-edge node)) 'not-root))

      ;; initiate recursion
      (check-recursively node (awhen (parent-edge node) (tail it)))
      
      ;; return nil if no problems encountered
      nil)))	     


	     

(defun add-new-child (node edge-label child-label &optional (position nil))
  "add-new-child NODE EDGE-LABEL CHILD-LABEL &optional POSITION.  Create a new edge whose tail is NODE and whose head is a newly created node (which is returned).  POSITION is a nonnegative integer and defaults to the current number of children of NODE.  The new child will be the POSITIONth (starting from the left, counting from 0) child of NODE."


  (let* ((child (make-node :node-label child-label))
	 (edge (make-edge node child edge-label)))
    
    (setf (parent-edge child) edge)
    (setf (child-edges node) (insert-at-position (child-edges node) edge position))
    child))

(defun get-child (node i)
  "get-child NODE I.  Get the Ith child of NODE."
  (let ((e (nth i (child-edges node))))
    (unless e
      (error 'nonexistent-child :node node :num i))
    (head e)))

(defun children (node)
  "children NODE.  Return list of children of NODE."
  (map 'list #'head (child-edges node)))


(defun get-ancestor (node i)
  "get-ancestor NODE I.  Return the Ith ancestor of NODE (where the 0th ancestor is NODE, the first ancestor is its parent, and so on).    Might throw a 'nonexistent-ancestor exception if not enough ancestors exist."
  (do ((current node (get-parent current))
       (l 0 (incf l)))
      ((>= l i) current)
    (unless current
      (error 'nonexistent-ancestor :node node :i i :depth l))))

(defun get-parent (node)
  "get-parent NODE.  Return the parent of NODE or nil if it doesn't have one."
  (awhen (parent-edge node)
	 (tail it)))

(defun get-descendant (node label-seq &key (key #'identity))
  "get-descendant NODE LABEL-SEQ &key (KEY #'identity).  Starting from NODE, follow a path towards the leaves with labels LABEL-SEQ and return the resulting node.  Return nil if there's no such path.  KEY is applied to the edge labels before comparison."
  (loop
      with current = node
      for l in label-seq
      for e = (get-child-edge-by-label current l :key key)
	      
      unless e 
      do (return nil)
	 
      do (setf current (head e))
      finally (return current)))

(defun depth (node)
  "depth NODE.  How many generations of ancestors does this node have
  (0 for root)."
  (do ((current node (get-parent current))
       (i -1 (incf i)))
      ((null current) i)))
	       

(defun get-child-edge-by-label (node label &key (key #'identity))
  "get-child-edge-by-label NODE LABEL &key KEY.  Return child edge with this label (tested using #'equal), or nil if there isn't one.  KEY is applied to edge labels before comparison."
  (find label (child-edges node) :key (lambda (x) (funcall key (edge-label x))) :test #'equal))

(defun get-root (tree)
  "get-root TREE.  Return the root node of TREE."
  ;; right now trees are represented by their root, so...
  tree)

(defun add-subtree (node edge-label tree &optional (position nil))
  "add-subtree NODE EDGE-LABEL TREE (POSITION nil).  Create a new outgoing edge from NODE with the given label.  Make the head of this edge be the root of TREE, and make the parent edge of the root be NODE.  POSITION is a nonnegative integer or nil - if it is nil it defaults to the current number of children of NODE.  The new child will be the POSITIONth (starting from the left) child of NODE."
  (let* ((root (get-root tree))
	 (edge (make-edge node root edge-label))
	 (p-edge (parent-edge root)))
    (assert (null p-edge) ()
      "Can't add a subtree with root ~a that already has a parent ~a"
      root (tail p-edge))
    (setf (child-edges node) (insert-at-position (child-edges node) edge position))
    (setf (parent-edge root) edge))
  (values))


(defun remove-subtree-below (node edge)
  "remove-subtree-below NODE OUTGOING-EDGE.  Has the effect of 'cutting' OUTGOING-EDGE.  Remove OUTGOING-EDGE from the list of child edges of NODE.  Also, set the PARENT-EDGE of the head of OUTGOING-EDGE to nil.  Finally, return the now disconnected subtree."
  (deletef (child-edges node) edge)
  (let ((subtree-root (head edge)))
    (setf (parent-edge subtree-root) nil)
    subtree-root))

(defun remove-subtree (node)
  "remove-subtree NODE.  Remove the subtree headed by NODE from its containing tree and return it."
  (let ((e (parent-edge node)))
    (remove-subtree-below (tail e) e)))

(defun leaf? (node)
  "leaf? NODE.  True if it's has no children."
  (= 0 (length (child-edges node))))

(defun num-nodes (tree)
  "num-nodes TREE.  Number of nodes."
  (1+ (loop for e in (child-edges tree) summing (num-nodes (head e)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; iteration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun postorder-iterator (tree)
  "postorder-iterator TREE. Return an iterator over the nodes of TREE, i.e. a function of no arguments which returns two values, NEXT-NODE and DONE."
  (let ((stack1 (list (list (get-root tree))))
	(stack2 nil))
    (lambda ()
      (while (car stack1)
	(let ((n (pop (car stack1))))
	  (push n stack2)
	  (push (children n) stack1)))
      (pop stack1)
      (if stack2
	  (values (pop stack2) nil)
	(values nil t)))))
	

(defmacro do-postorder ((var tree &optional result-form num-var) &rest body)
  "do-postorder (VAR TREE &optional RESULT-FORM NUM-VAR) &rest BODY.
Repeatedly execute BODY with VAR bound to the successive nodes of tree in postorder.  Finally return RET-VAL."
  `(do-iterator (postorder-iterator ,tree) (,var ,result-form ,num-var) ,@body))

(defgeneric map-postorder (result-type fn tree)
  (:documentation "map-postorder RESULT-TYPE FN TREE.  Iterate over the nodes of TREE in postorder and apply FN to each one, collecting the results into an object of type RESULT-TYPE (which is either 'list or 'vector).")
  (:method ((result-type (eql 'list)) fn tree)
	   (map-iterator-to-list fn (postorder-iterator tree)))
  (:method ((result-type (eql 'vector)) fn tree)
	   (let ((v (make-array (num-nodes tree))))
	     (do-postorder (x tree v i)
	       (setf (aref v i) (funcall fn x))))))



(defun preorder-iterator (tree)
  "preorder-iterator TREE.  Return an iterator over the nodes of TREE, i.e. a function of no arguments which returns two values, NEXT-NODE and DONE."
  (let ((stack nil))
    (push (list (get-root tree)) stack)
    (lambda ()
      (if stack
	  (let ((next (pop (car stack))))
	    (when (null (car stack))
	      (pop stack))
	    (awhen (children next)
		   (push it stack))
	    (values next nil))
	(values nil t)))))

(defmacro do-preorder ((var tree &optional result-form num-var) &rest body)
  "do-preorder (VAR TREE &optional RESULT-FORM NUM-VAR) &rest BODY.
Repeatedly execute BODY with VAR bound to the successive nodes of tree in preorder.  Finally return RET-VAL."
  `(do-iterator (preorder-iterator ,tree) (,var ,result-form ,num-var) ,@body))

(defgeneric map-preorder (result-type fn tree)
  (:documentation "map-preorder RESULT-TYPE FN TREE.  Iterate over the nodes of TREE in preorder and apply FN to each one, collecting the results into an object of type RESULT-TYPE (which is either 'list or 'vector).")
  (:method ((result-type (eql 'list)) fn tree)
	   (map-iterator-to-list fn (preorder-iterator tree)))
  (:method ((result-type (eql 'vector)) fn tree)
	   (let ((v (make-array (num-nodes tree))))
	     (do-preorder (x tree v i)
	       (setf (aref v i) (funcall fn x))))))
	   


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; copying
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun copy-subtree-below (edge)
  "copy-subtree EDGE.  Return a copy of the subtree consisting of the head node of edge and its descendants.  A copy of the subtree is made, so it can be modified without affecting the original.  Also, the parent node of the root of the new subtree is set to nil"
  (copy-subtree (head edge)))


(defun copy-subtree (node)
  "copy-subtree TREE.  Return a fresh copy of tree which can be modified without affecting the old one.  The parent of TREE The node and edge labels are copied using clone.  Note that even if TREE is a subtree, i.e., if its root has some other node as its parent, the returned TREE's root will not have a parent."
  
  (labels
      ((copy-tree-rec (node par par-edge-label)
	 (let ((new-node (make-node :node-label (clone (node-label node)))))
	   (when par
	     (setf (parent-edge new-node)
	       (make-edge par new-node (clone par-edge-label))))
	   (setf (child-edges new-node)
	     (mapcar
	      (lambda (e)
		(let ((new-child (copy-tree-rec (head e) new-node
						(edge-label e))))
		  (parent-edge new-child)))
	      (child-edges node)))
	   new-node)))
    
    (copy-tree-rec node nil nil)))
				    
    




  
  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; printing, debugging
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; not used any more?
(defmethod print-object ((n node) str)
  (format str "(Node ~a with parent ~a and children ~a)"
	  (node-label n)
	  (let ((p (parent-edge n)))
	    (when p
	      (node-label (tail p))))
	  (map 'vector (lambda (e) (node-label (head e))) (child-edges n))))

;; not used any more?
(defmethod print-object ((e edge) str)
  (format str "<<Edge ~a from ~a to ~a>>"
	  (edge-label e) (node-label (tail e))
	  (node-label (head e))))


(defun pprint-edge (str e)
  (pprint-logical-block (str nil)
    (format str "(Edge ~a " (edge-label e))
    (pprint-newline :fill str)
    (format str "from ~a " (node-label (tail e)))
    (pprint-newline :fill str)
    (format str "to ~a" (node-label (head e)))))


(defun pprint-node (str n)
  (pprint-logical-block (str nil)
    (format str "<<Node ~a " (node-label n))
    (pprint-newline :fill str)
    (let ((p (parent-edge n)))
      (when p
	(format str "Parent ~a " (node-label (tail p)))
	(pprint-newline :fill str)))
    (format str " with child labels ")
    (pprint-fill str (map 'list #'edge-label (child-edges n)))))

(set-pprint-dispatch 'node #'pprint-node)
(set-pprint-dispatch 'edge #'pprint-edge)
		    



(defun move-par ()
  "move-par 
Move current node up tree."
  (if *current-node*
      (aif (get-parent *current-node*)
	   (setf *current-node* it)
	   (warn "Node ~a does not have a parent." (node-label *current-node*)))
    (warn "*current-node* is nil."))
  *current-node*)

(defun move-child (i)
  "move-child I
Move to child I of current node."
  (if *current-node*
      (handler-case 
	  (setf *current-node*
	    (get-child *current-node* i))
	(nonexistent-child ()
	  (warn "Child ~a does not exist" i)))
    (warn "*current-node* is nil"))
  *current-node*)

(defun print-current-node ()
  (format t "~a" *current-node*))

(defun print-tree-upto (n)
  (let ((depth (depth n)))
    (print-tree (get-ancestor n depth) (max depth (or *max-print-depth* 0)))))



(defun print-tree (node &optional (max-depth *max-print-depth*) (str t))
  "print-tree TREE &optional (MAX-DEPTH *max-print-depth*) (STREAM t)"

  (let ((prob (is-inconsistent-tree node nil)))
    (assert (not prob) ()
      "Cannot print tree rooted at ~a as it is not consistent for reason ~a"
      node prob))
  (let ((nodes-seen nil)
	(printed-last-time t))
    (labels
	((print-tree-rec (node level)
	   (assert (not (member node nodes-seen)) ()
	     "Cycle : node ~a seen twice" (node-label node))
	   (push node nodes-seen)
	   
	   (if (aand max-depth (< it level))
	       (when printed-last-time
		 (setf printed-last-time nil)
		 (format str "~&~v@T..." level))
	     (progn
	       (setf printed-last-time t)
		 
	       ;; print node at right level, and possibly a * after it
	       (format str "~&~v@T~a~:[~; *~]" level 
		       (node-label node) (eq node *current-node*))
	   
	       ;; recurse
	       (dolist (e (child-edges node))
		 (print-tree-rec (head e) (1+ level)))))))
    
      (print-tree-rec node 0)
      (values))))