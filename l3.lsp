; recursive remove all elements of a value from list
(defun fr (i l) 
    (if (null l) 
	() 
	(if (eq (first l) i) 
	    (fr i (rest l)) 
	    (cons (first l) (fr i (rest l)))
	)
    )
)

; iterative remove all elements of a value from list
(defun fi (i l)
    (let ((s nil))
	(dolist (o l)
	    (if (eq o i) 
		() 
		(setf s (append s (list o)))
	    )
	)
	s
    )
)

; iterative intersection of two sets
(defun setinter(l1 l2)
    (let ((s nil))
	(dolist (o l1)
	    (if (and (member o l2) (not (member o s))) 
		(setf s (append s (list o))) 
		()
	    )
	)
	s
    )
)

; iterative union of two sets
(defun setunion (l1 l2)
    (let ((s l1))
	(dolist (o l2)
	    (if (not (member o l1))
		(setf s (append s (list o)))
		()
	    )
	)
	s
    )
)

; iterative difference of two sets
(defun setdifference (l1 l2)
    (let ((s nil))
	(dolist (o l1)
	    (if (not (member o l2))
		(setf s (append s (list o)))
		()
	    )
	)
	s
    )
)

; iterative infix expression calculator
(defun infix (l)
    (if (null l)
	()
	(let ((q (first l)) (s nil))
	    (dolist (o l)
		(if (listp o) 
		    (setf s (append s (list q) (list (infix o)))) 
		    (setf s (append s (list q) (list o)))
		)
	    )
	    (cdddr s)
	)
    )
)
