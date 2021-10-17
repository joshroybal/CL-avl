(load "avl.lisp")
;;; avl-tree summary report
(defun report (bst)
  (format t "~&~S" bst)
  (format t "~&pre-order")
  (pre-order #'display-node bst)
  (format t "~&in-order")
  (in-order #'display-node bst)
  (format t "~&post-order")
  (post-order #'display-node bst)
  (format t "~&in-order predecessor node successor")
  (in-order
   #'(lambda (x) (format t "~&~S ~S ~S"
			 (key-and-value (predecessor-node (key x) bst))
			 (key-and-value x)
			 (key-and-value (successor-node (key x) bst))))
   bst)
  (format t "~&nodes = ~d" (count-nodes bst))
  (format t "~&leaves = ~d" (count-leaves bst))
  (format t "~&height = ~d" (height bst)))

;;; tests on smallest data set
(defvar *colors*)
(text-file->bst "colors.txt" *colors*)
(report *colors*)

;;; tests on medium data set
(defvar *states*)
(text-file->bst "states.txt" *states*)
(report *states*)

;;; tests on largest data set
(defvar *countries*)
(text-file->bst "countries.txt" *countries*)
(report *countries*)
(bst-remove '|UNITED KINGDOM; ENGLAND| *countries*)
(bst-remove '|UNITED KINGDOM NORTHERN IRELAND| *countries*)
(bst-insert '|UNITED KINGDOM| 'LONDON *countries*)
(bst-insert 'ENGLAND 'LONDON *countries*)
(report *countries*)
