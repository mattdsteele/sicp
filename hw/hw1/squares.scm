(define (squares list)
  (if (equal? list '())
      '()
      (sentence
       (square (first list))
       (squares (butfirst list)))))

(define (square x)
  (* x x))
