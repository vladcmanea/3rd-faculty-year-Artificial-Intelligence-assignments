; words in a phrase
(defun phr (l d)
    (let ((s ""))
	(dolist (o l)
	    (if (eq o (first l))
		(if (eq o (car (last l)))
		    (setf s (concatenate 'string s (string-capitalize (cdr (assoc o d :test #'equalp))) "."))
		    (setf s (concatenate 'string s (string-capitalize (cdr (assoc o d :test #'equalp))) " "))
		)
		(if (eq o (car (last l)))
		    (setf s (concatenate 'string s (string-downcase (cdr (assoc o d :test #'equalp))) "."))
		    (setf s (concatenate 'string s (string-downcase (cdr (assoc o d :test #'equalp))) " "))
		)
	    )
	)
	s
    )
)

; building structure
(defstruct 
    (bdg 
	(:print-object 
	    (lambda (n stream) 
		(format stream "[Cladirea cu numele ~A cu ~A etaje]" (bdg-name n) (bdg-flrs n))
	    )
	)
    )
    name 
    flrs
)

; street structure
(defstruct
    (str
	(:print-object 
	    (lambda (n stream) 
		(format stream "[Strada ~A cu cladirile ~A]" (str-name n) (str-bdgs n))
	    )
	)
    )
    name
    bdgs
)

(defun src (b s)
    (let ((r (- 0 1)) (v (str-bdgs s)))
	(loop for i from 0 to (- (length v) 1) by 1 do
    	    (if (and (string-equal (bdg-name (aref v i)) (bdg-name b)) (eq (bdg-flrs (aref v i)) (bdg-flrs b)))
	        (setf r i) 
	        ()
    	    )
	)
	r
    )
)

; set streets
(setf street (make-str :name "Podu Ros" :bdgs (vector (make-bdg :name "Palat" :flrs 3) (make-bdg :name "Mall" :flrs 10) (make-bdg :name "Moldova" :flrs 22))))

; set buildings
(setf queryok (make-bdg :name "Moldova" :flrs 22))
(setf querynotok (make-bdg :name "Moldova" :flrs 23))

; building structure
(defstruct 
    (bdg1 
	(:print-object 
	    (lambda (n stream) 
		(format stream "[Cladirea de tipul ~A cu numele ~A]" (bdg1-type n) (bdg1-name n))
	    )
	)
    )
    type
    name 
)

; street structure
(defstruct
    (str1
	(:print-object 
	    (lambda (n stream) 
		(format stream "[Strada ~A cu cladirile ~A]" (str1-name n) (str1-bdgs n))
	    )
	)
    )
    name
    bdgs
)

; binary search function (must have an order!)
(defun bsearch (p v l r)
    (if (> l r) 
	(make-bdg1 :type p :name "INEXISTENT") 
	(let ((q nil) (m (floor (/ (+ l r) 2))))
	    (if (string= p (bdg1-type (aref v m)))
		(setf q (aref v m))
		(if (string< p (bdg1-type (aref v m)))
		    (setf q (bsearch p v l (- m 1)))
		    (setf q (bsearch p v (+ m 1) r))
		)
	    )
	    q
	)
    )
)

; binary search wrapper
(defun findin (p s)
    (bsearch p (str1-bdgs s) 0 (length (str1-bdgs s)))
)

; street of buildings
(setf st (make-str1 :name "Podu Ros" :bdgs (vector 
    (make-bdg1 :type "aquarium" :name "Aqua1") 
    (make-bdg1 :type "bar" :name "Bar2") 
    (make-bdg1 :type "canteen" :name "Bar3") 
    (make-bdg1 :type "edifice" :name "Bar3") 
    (make-bdg1 :type "farm" :name "sheep") 
    (make-bdg1 :type "pharmacy" :name "ecofarm") 
    (make-bdg1 :type "restaurant" :name "magicpizza") 
)))

; graph shortest path
(defun path (graph start finish)
	(let* (
		(n (length graph)) 
		(visited nil) 
		(father nil) 
		(answer nil)
		(queue (list start))
	    )
	    (progn
		(loop for i from 0 to (- n 1) by 1 do 
		    (progn
			(setf visited (cons n visited))
			(setf father (cons (- 0 1) father))
		    )
		)
		(setf (nth start visited) 0)
		(loop while (> (list-length queue) 0) do
		    (progn
			(let* (
				(u (car queue))
				(friends (nth u graph))
				(m (length friends))
			    )
			    (loop for i from 0 to (- m 1) by 1 do
				(let* (
					(v (nth i friends))
				    )
				    (if (eq (nth v visited) n)
					(progn
					    (setf (nth v visited) (+ 1 (nth u visited)))
					    (setf (nth v father) u)
					    (setf queue (append queue (list v)))
					)
					()
				    )
				)
			    )
			)
			(setf queue (rest queue))
		    )
		)
		(if (eq (nth finish visited) n)
		    (setf answer "Could not reach destination node.")
		    (let ((i finish))
			(loop while (/= i (- 0 1)) do
			    (progn
				(setf answer (append (list i) answer))
				(setf i (nth i father))
			    )
			)
		    )
		)
	    )
	    answer
	)
)

(setf graph (list (list 1 3) (list 3 2) (list 1) (list 4) (list 5) ()))
