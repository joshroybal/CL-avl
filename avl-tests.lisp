(load "avl")
;;; very small test - 7 nodes
(format t "~&very small test - 7 nodes")
(format t "~&(setf colors (text-file->tree \"colors.txt\"))")
(setf colors (text-file->tree "colors.txt"))
(format t "~&~S" colors)
(format t "~&(count-nodes colors) -> ~d" (count-nodes colors))
(format t "~&(count-leaves colors) -> ~d" (count-leaves colors))
(format t "~&(height colors) -> ~d" (height colors))
(format t "~&(equalp (deserialize (serialize colors)) colors) -> ")
(format t "~d" (equalp (deserialize (serialize colors)) colors))
(format t "~&(serialize-to-file \"colors.dat\")")
(serialize-to-file colors "colors.dat")
(format t "~&(equalp (deserialize-from-file \"colors.dat\") colors) -> ")
(format t "~S~%~%" (equalp (deserialize-from-file "colors.dat") colors))
;;; small test - 100 nodes
(format t "~&small test - 100 nodes")
(format t "~&(setf C (text-file->tree \"c.txt\"))")
(setf C (text-file->tree "c.txt"))
(format t "~&(count-nodes C) -> ~d" (count-nodes C))
(format t "~&(count-leaves C) -> ~d" (count-leaves C))
(format t "~&(height C) -> ~d" (height C))
(format t "~&(equalp (deserialize (serialize colors)) C) -> ")
(format t "~d" (equalp (deserialize (serialize C)) C))
(format t "~&(serialize-to-file \"c.dat\")")
(serialize-to-file C "c.dat")
(format t "~&(equalp (deserialize-from-file \"c.dat\") C) -> ")
(format t "~S~%~%" (equalp (deserialize-from-file "c.dat") C))
;;; medium test - 500 nodes
(format t "~&medium test - 500 nodes")
(format t "~&(setf D (text-file->tree \"d.txt\"))")
(setf D (text-file->tree "d.txt"))
(format t "~&(count-nodes D) -> ~d" (count-nodes D))
(format t "~&(count-leaves D) -> ~d" (count-leaves D))
(format t "~&(height D) -> ~d" (height D))
(format t "~&(equalp (deserialize (serialize D)) D) -> ")
(format t "~d" (equalp (deserialize (serialize D)) D))
(format t "~&(serialize-to-file \"d.dat\")")
(serialize-to-file d "d.dat")
(format t "~&(equalp (deserialize-from-file \"d.dat\") D) -> ")
(format t "~S~%~%" (equalp (deserialize-from-file "d.dat") D))
;;; largest test - 1000 nodes
(format t "~&largest test - 1000 nodes")
(format t "~&(setf M (text-file->tree \"m.txt\"))")
(setf M (text-file->tree "m.txt"))
(format t "~&(count-nodes M) -> ~d" (count-nodes M))
(format t "~&(count-leaves M) -> ~d" (count-leaves M))
(format t "~&(height M) -> ~d" (height M))
(format t "~&(equalp (deserialize (serialize M)) M) -> ")
(format t "~d" (equalp (deserialize (serialize M)) M))
(format t "~&(serialize-to-file \"m.dat\")")
(serialize-to-file M "m.dat")
(format t "~&(equalp (deserialize-from-file \"m.dat\") M) -> ")
(format t "~S" (equalp (deserialize-from-file "m.dat") M))