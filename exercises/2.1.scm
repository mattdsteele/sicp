(define (make-rat n d)
  (if (negative? d)
      (make-rat (* n -1) (* d -1))
      (cons n d)))