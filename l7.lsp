;
;	functia euristica
;	primeste ca parametru o configuratie curenta si o configuratie finala
;	configuratia curenta este o lista de forma ((123)(123)(123))
;
(defun heuristic (conf)
    (let
	(
	    (ans 0)
	)
	(loop for i from 0 to 2 by 1 do
	    (loop for j from 0 to 2 by 1 do
		(let*
		    (
			(c (nth j (nth i conf)))
			(x (car (nth c *finconf*)))
			(y (cdr (nth c *finconf*)))
			(h1 (if (and (eq i x) (eq j y)) 0 1))
			(h2 (+ (abs (- i x)) (abs (- j y))))
		    )
		    (progn
			(setf ans (+ ans (* h1 h2)))
		    )
		)
	    )
	)
	ans
    )
)

;
;	functia de mutare a blancului
;	primeste la intrare o stare ((123)(123)(123))
;	0 - muta blancul in sus
;	1 - muta blancul la dreapta
;	2 - muta blancul in jos
;	3 - muta blancul in stanga
;
(defun movg (conf dir)
    (let*
	(
	    (x 0)
	    (y 0)
	    (final (list (list 0 0 0) (list 0 0 0) (list 0 0 0)))
	)
	(progn
	    (loop for i from 0 to 2 by 1 do
		(loop for j from 0 to 2 by 1 do
		    (progn
			(setf (nth j (nth i final)) (nth j (nth i conf)))
		        (if (eq 0 (nth j (nth i final)))
			    (progn
				(setf x i)
				(setf y j)
			    )
			    ()
			)
		    )
		)
	    )
	    ; heading to the top
	    (if (and (eq dir 0) (not (eq x 0)))
		(progn
		    (setf (nth y (nth x final)) (nth y (nth (- x 1) final)))
		    (setf (nth y (nth (- x 1) final)) 0)
		)
		()
	    )
	    ; heading to the right
	    (if (and (eq dir 1) (not (eq y 2)))
		(progn
		    (setf (nth y (nth x final)) (nth (+ y 1) (nth x final)))
		    (setf (nth (+ y 1) (nth x final)) 0)
		)
		()
	    )
	    ; heading to the bottom
	    (if (and (eq dir 2) (not (eq x 2)))
		(progn
		    (setf (nth y (nth x final)) (nth y (nth (+ x 1) final)))
		    (setf (nth y (nth (+ x 1) final)) 0)
		)
		()
	    )
	    (if (and (eq dir 3) (not (eq y 0)))
		(progn
		    (setf (nth y (nth x final)) (nth (- y 1) (nth x final)))
		    (setf (nth (- y 1) (nth x final)) 0)
		)
		()
	    )
	)
	final
    )
)

;
;	functia best first gaseste cat poate de repede solutia folosind o euristica :)
;	functia primeste o stare initiala si o stare finala :)
;
(defun path (infile outfile)
	(let* (
		(father nil)
		(answer nil)
		(queue nil)
		(iniconf nil)
		(finalconf nil)
	    )
	    (progn
		(with-open-file (fin infile :direction :input)
		
		    (loop for i from 0 to 2 by 1 do
			(progn
			    (setf queue nil)
			    (loop for j from 0 to 2 by 1 do
				(setf queue (append queue (list (read fin))))
			    )
			    (setf iniconf (append iniconf (list queue)))
			)
		    )
		    (loop for i from 0 to 2 by 1 do
			(progn
			    (setf queue nil)
			    (loop for j from 0 to 2 by 1 do
				(setf queue (append queue (list (read fin))))
			    )
			    (setf finalconf (append finalconf (list queue)))
			)
		    )
		)
		(format t "~A ~A~%" iniconf finalconf)
		(setf queue (list iniconf))
		(defparameter *finconf* nil)
		(loop for i from 0 to 8 by 1 do
		    (setf *finconf* (acons 0 0 *finconf*))
		)
		(loop for i from 0 to 2 by 1 do
		    (loop for j from 0 to 2 by 1 do
			(progn
			    (setf (car (nth (nth j (nth i finalconf)) *finconf*)) i)
			    (setf (cdr (nth (nth j (nth i finalconf)) *finconf*)) j)
			)
		    )
		)
		(setf father (acons iniconf nil father))
		(loop while (and (> (list-length queue) 0) (eq nil (assoc finalconf father :test #'equalp))) do
		    (progn
			(let*
			    (
				(u (car queue))
			    )
			    (progn 
				(loop for m from 0 to 3 by 1 do
				    (let*
					(
					    (v (movg u m))
					)
					(progn
					    (if (assoc v father :test #'equalp)
						()
						(progn
						    (format t "+")
						    (setf queue (append queue (list v)))
						    (setf father (acons v u father))
						)
					    )
					)
				    )
				)
			    )
			)
			(format t "-")
			(setf queue (rest queue))
			(sort queue #'< :key #'heuristic)
		    )
		)
		(with-open-file (fout outfile :direction :output)
		    (if (assoc finalconf father :test #'equalp)
			(let 
			    (
				(i finalconf)
			    )
			    (progn
				(loop while (not (eq nil i)) do
				    (progn
					(setf answer (append (list i) answer))
					(setf i (cdr (assoc i father :test #'equalp)))
				    )
				)
				(loop for i from 0 to (- (length answer) 1) by 1 do
				    (let
					(
					    (state (nth i answer))
					)
					(format fout "State ~A:~%~A~A~A~%~A~A~A~%~A~A~A~%~%"
					    i
					    (nth 0 (nth 0 state))
					    (nth 1 (nth 0 state))
					    (nth 2 (nth 0 state))
					    (nth 0 (nth 1 state))
					    (nth 1 (nth 1 state))
					    (nth 2 (nth 1 state))
					    (nth 0 (nth 2 state))
					    (nth 1 (nth 2 state))
					    (nth 2 (nth 2 state))
					)
				    )
				)
			    )
			)
			(format fout "Could not reach destination node.")
		    )
		)
	    )
	)
)
