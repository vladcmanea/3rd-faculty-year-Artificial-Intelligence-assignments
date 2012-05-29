;
;	function for slope
;
(defun slope (xt yt xb yb)
    (if (and (< -0.0000000001 (abs (- yt yb))) (< (abs (- yt yb)) 0.0000000001))
	(/ 1 0.0000000001)
	(abs (/ (- xt xb) (- yt yb)))
    )
)

;
;	function for distance
;
(defun dist (xti xtf xbi xbf)
    (let
	(
	    (e (abs (- (abs (- xtf xti)) (abs (- xbf xbi)))))
	)
	()
	(* e e e)
    )
)

;
;	segments optimal path problem
;	uses dynamic programming algorithm
;
;	Smin[i][j] = 	the minimum cost of having elements from 1..i and 1..j connected and (i, j) is alignment
;			seen as the sum of alignment slopes and the absolute differences in size
;
;	Smin[i][j] = min
;			{
;				Smin[i - k][j - l] + |(Xt[i] - Xt[i - k]) - (Xb[j] - Xb[j - l])| + slope(i, j)
;			}
;
;	Smin[0][0] = initial value = slope of the alignment (0, 0)
;	Smin[N - 1][N - 1] = final value = problem solution
;
(defun opt (src dest)
    (let*
	(
	    (n 0)
	    (m 0)
	    (a 0)
	    (b 0)
	    (Xt nil)
	    (Yt nil)
	    (Xb nil)
	    (Yb nil)
	    (Smin nil)
	)
	(progn
	    (with-open-file (fin src :direction :input)
		(progn
		    (setf n (read fin))
		    (format t "N=~A~%" n)
		    (setf m (read fin))
		    (format t "M=~A~%" m)
		    (setf a (read fin))
		    (format t "A=~A~%" a)
		    (setf b (read fin))
		    (format t "B=~A~%" b)
		    (setf Xt (make-array n :initial-element 0))
		    (setf Yt (make-array n :initial-element 0))
		    (setf Xb (make-array m :initial-element 0))
		    (setf Yb (make-array m :initial-element 0))
		    (loop for i from 0 to (- n 1) by 1 do
			(progn
			    (setf (aref Xt i) (read fin))
			    (setf (aref Yt i) (read fin))
			)
		    )
		    (loop for j from 0 to (- m 1) by 1 do
			(progn
			    (setf (aref Xb j) (read fin))
			    (setf (aref Yb j) (read fin))
			)
		    )
		)
	    )
	    (setf Smin (make-array (list n m) :initial-element 0))
	    (setf (aref Smin 0 0) (* a (slope (aref Xt 0) (aref Yt 0) (aref Xb 0) (aref Yb 0))))
	    (loop for i from 1 to (- n 1) by 1 do
		(loop for j from 1 to (- m 1) by 1 do
		    (progn
			(setf (aref Smin i j)
			    (+
				(* a (slope (aref Xt i) (aref Yt i) (aref Xb j) (aref Yb j)))
				(* b (dist (aref Xt i) (aref Xt 0) (aref Xb j) (aref Xb 0)))
				(aref Smin 0 0)
			    )
			)
			(loop for k from 0 to (- i 1) by 1 do
			    (loop for l from 0 to (- j 1) by 1 do
				(let
				    (
					(c
					    (+
						(* a (slope (aref Xt i) (aref Yt i) (aref Xb j) (aref Yb j)))
						(* b (dist (aref Xt i) (aref Xt k) (aref Xb j) (aref Xb l)))
						(aref Smin k l)
					    )
					)
				    )
				    (if (< c (aref Smin i j))
					(setf (aref Smin i j) c)
					()
				    )
				)
			    )
			)
		    )
		)
	    )
	    (with-open-file (fout dest :direction :output)
		(format fout "~A~%" (aref Smin (- n 1) (- m 1)))
	    )
	)
    )
)
