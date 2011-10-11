(define (last-pair l)
  (if (null? (cdr l))
      l
      (last-pair (cdr l))))