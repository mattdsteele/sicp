(define (compose x y)
  (lambda (z) (x (y z))))

(define (square x) (* x x))
(define (inc x) (+ x 1))