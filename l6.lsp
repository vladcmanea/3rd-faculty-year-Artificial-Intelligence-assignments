; copy from one file to another
(defun cp (source destination &optional (comment "%"))
    (with-open-file (fin source :direction :input :if-does-not-exist nil)
	(with-open-file (fout destination :direction :output)
	    (progn
		(setf line (read-line fin nil nil))
		(loop while (not (eq line nil)) do
		    (progn
			(if (or (< (length line) (length comment)) (and (>= (length line) (length comment)) (not (string= comment (subseq line 0 (length comment))))))
			    (format fout "~A~%" line)
			    (format fout "~%")
			)
			(setf line (read-line fin nil nil))
		    )
		)
	    )
	)
    )
)

; structure to hold the state
(defstruct state
    can
    mis
    bot
)

;
; canibals and missionaries problem
; tested on instance (3 2) on
; www.novelgames.com/flashgames/game.php?id=54
;
(defun cm (s k)
    (let*
	(
	    (inistate (make-state :can s :mis s :bot nil))
	    (finstate (make-state :can 0 :mis 0 :bot t))
	    (visited (list inistate))
	    (fathers (pairlis (list inistate) (list nil)))
	    (queue (list inistate))
	)
	(progn
	    (loop while (and (> (length queue) 0) (not (member finstate visited))) do
		(let*
		    (
			(e (nth 0 queue))
			(m (state-mis e))
			(c (state-can e))
			(g (state-bot e))
		    )
		    (progn
			; i people in the boat
			(loop for i from 1 to k do
			    ; j canibals in the boat, i-j missionaries in the boat
			    (loop for j from 0 to i do
				(let*
				    (
					(a j)
					(b (- i j))
				    )
				    (if (eq g nil)
					; boat is on the left side
					(if (and (>= c a) (>= m b))
					    ; a canibals and b missionaries can be transported
					    (if (or (>= (- m b) (- c a)) (= m b))
						; the left side is safe
						(if (or (>= (- (+ s b) m) (- (+ s a) c)) (= (+ s b) m))
						    ; the right side is safe
						    (let*
							(
							    (w (make-state :can (- c a) :mis (- m b) :bot t))
							)
							(if (eq nil (find w visited :test #'equalp))
							    ; a new state, hey :)
							    (progn
								(setf queue (append queue (list w)))
								(setf visited (append visited (list w)))
								(setf fathers (acons w e fathers))
							    )
							    ()
							)
						    )
						    ()
						)
						()
					    )
					    ()
					)
					; boat is on the right side
					(if (and (>= (- s c) a) (>= (- s m) b))
					    ; a canibals and b missionaries can be transported
					    (if (or (>= (+ m b) (+ c a)) (= 0 (+ m b)))
						; the left side is safe
						(if (or (>= (- s (+ m b)) (- s (+ c a))) (= (- s (+ m b)) 0))
						    ; the right side is safe
						    (let*
							(
							    (w (make-state :can (+ c a) :mis (+ m b) :bot nil))
							)
							(if (eq nil (find w visited :test #'equalp))
							    ; a new state, hey :)
							    (progn
								(setf queue (append queue (list w)))
								(setf visited (append visited (list w)))
								(setf fathers (acons w e fathers))
							    )
							    ()
							)
						    )
						    ()
						)
						()
					    )
					    ()
					)
				    )
				)
			    )
			)
			(setf queue (rest queue))
		    )
		)
	    )
	)
	(if (eq nil (find finstate visited :test #'equalp))
	    ; final state not reached
	    (format t "The final state was not found.~%")
	    ; final state reached
	    (let*
		(
		    (state finstate)
		    (conf nil)
		    (steps nil)
		)
		(progn
		    (format t "The final state was found:~%")
		    (loop while (not (eq nil state)) do
			(progn
			    (setf conf (assoc state fathers :test #'equalp))
			    (setf steps (acons (cdr conf) (car conf) steps))
			    (setf state (cdr conf))
			)
		    )
		    (loop for i from 1 to (- (length steps) 1) do
			(format t "They made a step from state (~A/~A, ~A/~A, ~A) to state (~A/~A, ~A/~A, ~A)~%" 
			    (state-mis (car (nth i steps))) (state-can (car (nth i steps)))
			    (- s (state-mis (car (nth i steps)))) (- s (state-can (car (nth i steps))))
			    (if (eq (state-bot (car (nth i steps))) nil) "L" "R")
			    (state-mis (cdr (nth i steps))) (state-can (cdr (nth i steps)))
			    (- s (state-mis (cdr (nth i steps)))) (- s (state-can (cdr (nth i steps))))
			    (if (eq (state-bot (cdr (nth i steps))) nil) "L" "R")
			)
		    )
		)
	    )
	)
    )
)
