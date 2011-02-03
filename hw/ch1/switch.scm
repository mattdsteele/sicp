(define (switch s)
  (switch-words s))

(define (switch-words s)
  (cond ((equal? s '()) ('()))
        ((is-you? (first s)) (sentence 'I (switch-remaining-words (bf s))))
        (else (switch-remaining-words s))))

(define (switch-remaining-words s)
  (if (equal? s '())
      '()
      (cond ((is-i-me? (first s))
	     (sentence 'you (switch-remaining-words (bf s))))
	    ((is-you? (first s))
	     (sentence 'me (switch-remaining-words (bf s))))
	    (else (sentence (first s) (switch-remaining-words (bf s)))))))

(define (is-i-me? wd)
  (or (equal? wd 'I) (equal? wd 'me)))

(define (is-you? wd)
  (equal? wd 'you))
