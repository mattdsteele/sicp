(define (double x)
  (lambda (y) (x (x y))))

(define (inc x) (+ x 1))