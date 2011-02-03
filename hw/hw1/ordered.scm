(define (ordered? numbers)
  (cond ((equal? numbers '()) #t)
	((equal? (butfirst numbers) '()) #t)
	((> (first numbers) (first (butfirst numbers))) #f)
	(else (ordered? (butfirst numbers)))))

