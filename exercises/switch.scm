(define (switch s)
  (switch-leading-me (basic-switch s)))

(define (switch-leading-me s)
  (if (equal? s '())
      '()
      (if (equal? (first s) 'me)
	  (sentence 'i (butfirst s))
	  s)))

(define (basic-switch s)
  (if (equal? s '())
      '()
      (cond ((is-i-me? (first s))
	     (sentence 'you (basic-switch (butfirst s))))
	    ((is-you? (first s))
	     (sentence 'me (basic-switch (butfirst s))))
	    (else (sentence (first s) (basic-switch (butfirst s)))))))

(define (is-i-me? wd)
  (or (equal? wd 'I) (equal? wd 'me)))

(define (is-you? wd)
  (equal? wd 'you))
