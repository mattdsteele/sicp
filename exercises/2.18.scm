(define (reverse l)
  (if (null? (cdr l))
      l
      (append (reverse (cdr l)) (cons (car l) nil))))

(define (new-append item list)
  (if (empty? item)
      list
      (cons (car item) (new-append (cdr item) list))))