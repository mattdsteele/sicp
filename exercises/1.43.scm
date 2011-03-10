(define (compose x y)
  (lambda (z) (x (y z))))

(define (square x) (* x x))
(define (inc x) (+ x 1))

(define (repeated f n)
  (define (repeated-iter i result)
    (if (= i n) result
        (repeated-iter (+ i 1) (compose f result))))
  (repeated-iter 1 f))