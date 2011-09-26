(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

#|
 | (cdr z)
 | (z (lambda (p q) q))
 | ((lambda (m) (m x y)) (lambda (p q) q))
 | ((lambda (p q) q) x y)
 | ^ a procedure with two arguments, and this takes the second of them
 |#
