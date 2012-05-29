; indexes addition
(defun index (l)
    (let
	(
	    (i (- 0 1))
	)
	(
	)
	(mapcar 
	    (lambda (x) 
		(progn
		    (setf i (+ i 1))
		    (cons x i)
		)
	    ) 
	    l
	)
    )
)

; median VMA macro
(defmacro medianv (x)
    `(if (eq 1 (mod (length ,x) 2))
	(nth (floor (/ (length ,x) 2)) (sort ,x #'<))
	(/ (+ (nth (- (/ (length ,x) 2) 1) (sort ,x #'<) ) (nth (/ (length ,x) 2) (sort ,x #'<))) 2)
    )
)

; median TVA macro
(defmacro mediant (x)
    `(let*
	(
	    (y (sort ,x #'<))
	    (l (length y))
	)
	()
	(if (eq 0 (mod l 2))
	    (/ (+ (nth (- (/ l 2) 1) y) (nth (/ l 2) y)) 2)
	    (nth (floor (/ l 2)) y)
	)
    )
)

; learning algorithm
(defun lrn ()
    (let
	(
	    (found nil)
	    (answer nil)
	)
	(progn
	    (if (boundp '*knowledge*)
		()
		(defparameter *knowledge* nil)
	    )
	    (if (listp *knowledge*)
		(progn
		    (dolist (bird *knowledge*)
			(if (eq found nil)
			    (progn
				(format t "Is it... ~A?~%" bird)
				(if (eq (read) 'yes)
				    (setf found t)
				    ()
				)
			    )
			    ()
			)
		    )
		    (if (eq found nil)
			(progn
			    (format t "What bird have you thought of?~%")
			    (setf answer (read))
			    (if (member answer *knowledge* :test #'equal)
				(format t "I already know the bird ~A~%" answer)
				(setf *knowledge* (cons answer *knowledge*))
			    )
			)
			()
		    )
		)
		()
	    )
	)
	(format t "Now I know these birds: ~A~%" *knowledge*)
    )
)

; bird structure
(defstruct bird
    name
    color
    size
    peak
)

; smarter learning algorithm
(defun sl ()
    (let*
	(
	    (left nil)
	    (right nil)
	    (query (make-bird :name nil :color nil :size nil :peak nil))
	    (cleft nil)
	    (cright nil)
	    (found 0)
	)
	(progn
	    (if (boundp '*smarter*)
		()
		(defparameter *smarter* nil)
	    )
	    (if (listp *smarter*)
		(progn 
		    (setf left 0)
		    (setf right (- (length *smarter*) 1))
		    (setf found 0)
		    (setf cleft left)
		    (setf cright left)
		    (loop for i from left to right by 1 do
			(progn
			    (setf j i)
			    (loop while (and (<= j right) (eq (bird-color (nth i *smarter*)) (bird-color (nth j *smarter*)))) do
				(setf j (+ 1 j))
			    )
			    (format T "Does the bird have the color ~A?~%" (bird-color (nth i *smarter*)))
			    (if (eq (read) 'yes)
				(progn
				    (setf found 1)
				    (setf cleft i)
				    (setf cright (- j 1))
				    (setf i right)
				)
				(setf i (- j 1))
			    )
			)
		    )
		    (if (eq found 0)
			(progn
			    (format T "Could not find a bird.~%")
			    (format T "Please provide a color:~%")
			    (setf (bird-color query) (read))
			    (format T "Please provide a size:~%")
			    (setf (bird-size query) (read))
			    (format T "Please provide a peak:~%")
			    (setf (bird-peak query) (read))
			    (format T "Please provide a name:~%")
			    (setf (bird-name query) (read))
			    (setf cleft nil)
			    (loop for i from 0 to right by 1 do
				(setf cleft (append cleft (list (nth i *smarter*))))
			    )
			    (setf cleft (append cleft (list query)))
			    (setf cleft (append cleft (nthcdr (+ 1 right) *smarter*)))
			    (setf *smarter* cleft)
			)
			(progn
			    (format T "left:~A right:~A~%" cleft cright)
			    (setf left cleft)
			    (setf right cright)
			    (setf (bird-color query) (bird-color (nth left *smarter*)))
			    (setf found 0)
			    (setf cleft left)
			    (setf cright left)
			    (loop for i from left to right by 1 do
				(progn
				    (setf j i)
				    (loop while (and (<= j right) (eq (bird-size (nth i *smarter*)) (bird-size (nth j *smarter*)))) do
					(setf j (+ 1 j))
				    )
				    (format T "Does the bird have the size ~A?~%" (bird-size (nth i *smarter*)))
				    (if (eq (read) 'yes)
					(progn
					    (setf found 1)
					    (setf cleft i)
					    (setf cright (- j 1))
					    (setf i right)
					)
					(setf i (- j 1))
				    )
				)
			    )
			    (if (eq found 0)
				(progn
				    (format T "Could not find a bird.~%")
				    (format T "Please provide a size:~%")
				    (setf (bird-size query) (read))
				    (format T "Please provide a peak:~%")
				    (setf (bird-peak query) (read))
				    (format T "Please provide a name:~%")
				    (setf (bird-name query) (read))
				    (setf cleft nil)
				    (loop for i from 0 to right by 1 do
					(setf cleft (append cleft (list (nth i *smarter*))))
				    )
				    (setf cleft (append cleft (list query)))
				    (setf cleft (append cleft (nthcdr (+ 1 right) *smarter*)))
				    (setf *smarter* cleft)
				)
				(progn
				    (format T "left:~A right:~A~%" cleft cright)
				    (setf left cleft)
				    (setf right cright)
				    (setf (bird-size query) (bird-size (nth left *smarter*)))
				    (setf found 0)
				    (setf cleft left)
				    (setf cright left)
				    (loop for i from left to right by 1 do
					(progn
					    (setf j i)
					    (loop while (and (<= j right) (eq (bird-peak (nth i *smarter*)) (bird-peak (nth j *smarter*)))) do
						(setf j (+ 1 j))
					    )
					    (format T "Does the bird have the peak ~A?~%" (bird-peak (nth i *smarter*)))
					    (if (eq (read) 'yes)
						(progn
						    (setf found 1)
						    (setf cleft i)
						    (setf cright (- j 1))
						    (setf i right)
						)
						(setf i (- j 1))
					    )
					)
				    )
				    (if (eq found 0)
					(progn
					    (format T "Could not find a bird.~%")
					    (format T "Please provide a peak:~%")
					    (setf (bird-peak query) (read))
					    (format T "Please provide a name:~%")
					    (setf (bird-name query) (read))
					    (setf cleft nil)
					    (loop for i from 0 to right by 1 do
						(setf cleft (append cleft (list (nth i *smarter*))))
					    )
					    (setf cleft (append cleft (list query)))
					    (setf cleft (append cleft (nthcdr (+ 1 right) *smarter*)))
					    (setf *smarter* cleft)
					)
					(progn
					    (format T "left:~A right:~A~%" cleft cright)
					    (setf left cleft)
					    (setf right cright)
					    (setf (bird-peak query) (bird-peak (nth left *smarter*)))
					    (progn
						(setf found 0)
						(loop for i from left to right by 1 do
						    (if (eq found 0)
							(progn
							    (format T "Is it the ~A bird?~%" (bird-name (nth i *smarter*)))
							    (if (eq (read) 'yes)
								(setf found 1)
								()
							    )
							)
							()
						    )
						)
						(if (eq found 0)
						    (progn
							(format T "Could not find a bird.~%")
							(format T "Please provide a name:~%")
							(setf (bird-name query) (read))
							(setf cleft nil)
							(loop for i from 0 to right by 1 do
							    (setf cleft (append cleft (list (nth i *smarter*))))
							)
							(setf cleft (append cleft (list query)))
							(setf cleft (append cleft (nthcdr (+ 1 right) *smarter*)))
							(setf *smarter* cleft)
						    )
						    ()
						)
					    )
					)
				    )
				)
			    )
			)
		    )
		)
		()
	    )
	)
	*smarter*
    )
)
