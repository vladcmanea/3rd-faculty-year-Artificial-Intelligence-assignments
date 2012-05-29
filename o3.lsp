; problem of n queens on a n x n table
(defun generate_queens (size many)
    (progn
	(defparameter *cols* (make-array size :initial-element 0))
	(defparameter *size* size)
	(defparameter *done* 0)
	(defparameter *many* many)
	(backtrack_queens 0)
	(format t "~%DONE")
    )
)

; backtracking procedure
(defun backtrack_queens (row)
    (if (< *done* *many*)
	(if (eq row *size*)
	    (progn
		(loop for orow from 0 to (- *size* 1) by 1 do
		    (progn
			(format t "~%")
			(loop for ocol from 0 to (- *size* 1) by 1 do
			    (if (eq ocol (aref *cols* orow))
				(format t "X")
				(format t "+")
			    )
			)
		    )
		)
		(format t "~%")
		(setf *done* (+ 1 *done*))
	    )
	    (loop for col from 0 to (- *size* 1) by 1 do 
		(let 
		    (
			(ok 1)
		    )
		    (progn
			(loop for orow from 0 to (- row 1) by 1 do
			    (if (or (eq col (aref *cols* orow)) (eq (abs (- row orow)) (abs (- (aref *cols* orow) col))))
				(setf ok 0)
				()
			    )
			)
			(if (eq ok 1)
			    (progn
    				(setf (aref *cols* row) col)
				(backtrack_queens (+ row 1))
			    )
			    ()
			)
		    )
		)
	    )
	)
	()
    )
)
