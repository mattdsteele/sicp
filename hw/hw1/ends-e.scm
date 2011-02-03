(define (ends-e s)
  (cond ((equal? s '()) '())
	((equal? (last (first s)) 'e) (sentence (first s) (ends-e (bf s))))
	(else (ends-e (bf s)))))
