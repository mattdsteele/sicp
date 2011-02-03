(define (ends-e s)
  (cond ((equal? s '()) '())
	((wd-ends-e (first s))
         (sentence (first s) (ends-e (bf s))))
	(else
         (ends-e (bf s)))))

(define (wd-ends-e w)
  (equal? (last w) 'e))
