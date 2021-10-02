(defun make-tree (data right left)
  (list data right left))

(defun make-leaf (data)
  (list data nil nil))

(defun left (node) (cadr node))
(defun right (node) (caddr node))

(defun leaf-p (tree)
  (and (listp tree) (null (left tree)) (null (right tree))))

(defun height (tree)
  (cond ((null tree) -1)
	(t (+ 1 (max (height (left tree)) (height (right tree)))))))

(defun balance-factor (node)
  (- (height (left node)) (height (right node))))

(defun balanced-p (node)
  (and (>= (balance-factor node) -1) (<= (balance-factor node) 1)))

(defun rotate-left (node)
  (make-tree
   (car (right node))
   (make-tree (car node) (left node) (left (right node)))
   (right (right node))))

(defun rotate-right (node)
  (make-tree
   (car (left node))
   (left (left node))
   (make-tree (car node) (right (left node)) (right node))))

(defun balance-node (node)
  (cond ((balanced-p node)
	 node)
	((> (balance-factor node) 1)
	 (cond ((> (balance-factor (left node)) 0)
		(rotate-right node))
	       (t
		(rotate-right
		 (make-tree
		  (car node)
		  (rotate-left (left node))
		  (right node))))))
	(t
	 (cond ((< (balance-factor (right node)) 0)
		(rotate-left node))
	       (t
		(rotate-left
		 (make-tree
		  (car node)
		  (left node)
		  (rotate-right (right node)))))))))		

(defun avl-insert (data node)
  (cond ((null node)
	 (make-leaf data))
	((equalp data (car node))
	 node)
	((string-lessp data (car node))
	 (balance-node
	  (make-tree
	   (car node)
	   (avl-insert data (left node))
	   (right node))))
	((string-greaterp data (car node))
	 (balance-node
	  (make-tree
	   (car node)
	   (left node)
	   (avl-insert data (right node)))))))
  
(defun node-p (data tree)
  (cond ((null tree) nil)
	((equalp data (car tree)) tree)
	((string-lessp data (car tree)) (node-p data (left tree)))
	((string-greaterp data (car tree)) (node-p data (right tree)))))

(defun count-nodes (tree)
  (cond ((null tree) 0)
	(t (+ 1 (count-nodes (left tree)) (count-nodes (right tree))))))
  
(defun serialize (tree)
  (cond ((null tree)
	 (list nil))
	(t
	 (append (list (car tree))
		 (serialize (left tree))
		 (serialize (right tree))))))

(defun deserialize (seq)
  (labels ((aux (x)
		(cond ((null x)
		       nil)
		      (t
		       (make-tree
			x
			(aux (pop seq))
			(aux (pop seq)))))))
	  (aux (pop seq))))

(defun serialize-to-file (tree filename)
  (let ((tree-list (serialize tree)))
    (with-open-file (outfile filename
				  :direction :output
				  :if-does-not-exist :create)
			 (format outfile "~S" tree-list))))

(defun deserialize-from-file (filename)
  (with-open-file (infile filename
			  :direction :input)
		  (deserialize (read infile))))

(defun pre-order (tree)
  (cond ((null tree) nil)
	(t
	 (format t "~&~S" (car tree))
	 (pre-order (left tree))
	 (pre-order (right tree)))))

(defun in-order (tree &optional port)
  (cond ((null tree) nil)
	(t
	 (in-order (left tree) port)
	 (if (null port)
	     (format t "~&~S" (car tree))
	   (format port "~A~%" (car tree)))
	 (in-order (right tree) port))))

(defun in-order-to-file (tree filename)
  (with-open-file (outfile filename
			   :direction :output
			   :if-does-not-exist :create)
		  (in-order tree outfile)))

(defun post-order (tree)
  (cond ((null tree) t)
	(t
	 (post-order (left tree))
	 (post-order (right tree))
	 (format t "~&~S" (car tree)))))

(defun count-leaves (tree)
  (cond ((null tree) 0)
	((leaf-p tree) 1)
	(t (+ (count-leaves (left tree)) (count-leaves (right tree))))))

(defun list->tree (list)
  (do ((rest list (cdr rest))
       (bst nil (avl-insert (car rest) bst)))
      ((null rest) bst)))

(defun text-file->tree (filename)
  (with-open-file (infile filename)
		  (do ((symbol (read infile nil 'eof) (read infile nil 'eof))
		       (bst nil (avl-insert symbol bst)))
		      ((eq symbol 'eof) bst)
		      (cond ((or (not (symbolp symbol)) (equalp symbol nil))
			     (format t "~&~S (NOT A SYMBOL)" symbol))))))
  
(defun bound (n) (* 1.44 (log n 2)))
