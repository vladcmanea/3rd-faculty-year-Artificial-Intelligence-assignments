(defun temp (x) 
    (cond 
	((< x -10) '(foarte frig))
	((< x 0) '(frig))
	((< x 10) '(moderat))
	((< x 20) '(cald))
	(t '(foarte cald))
    )
)

(defun g (x y)
    (cond
	((null y) x)
	((member (first y) (rest y)) (if (member (first y) x) (g x (rest y)) (g (cons (first y) x) (rest y))))
	(t (g x (rest y)))
    )
)

(defun count2 (x) 
    (g nil x)
)

(defun testprim (d x)
    (if (>= d x) 1 (if (eq (mod x d) 0) 0 (testprim (+ d 1) x)))
)

(defun primes (x)
    (cond
	((null x) 0)
	(t (if (atom (first x)) (+ (testprim 2 (first x)) (primes (rest x))) (+ (primes (first x)) (primes (rest x)))))
    )
)

(defun p (x)
    (cond
	((null x) nil)
	((numberp (first x)) (cons (first x) (p (rest x))))
	(t nil)
    )
)

(defun h (x y)
    (cond
	((null y) x)
	(t (if (< (list-length (p y)) (list-length x)) (h x (rest y)) (h (p y) (rest y))))
    )
)

(defun secv (x)
    (h nil x)
)