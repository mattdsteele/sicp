(define (compose x y)
  (lambda (z) (x (y z))))

(define (square x) (* x x))
(define (inc x) (+ x 1))

(define (repeated f n)
  (define (repeated-iter i result)
    (if (= i n) result
        (repeated-iter (+ i 1) (compose f result))))
  (repeated-iter 1 f))

(define (smooth f)
  (define dx .1)
  (define (avg a b c) (/ (+ a b c) 3))
  (lambda (x)
    (avg
        (f (- x dx))
        (f x)
        (f (+ x dx)))))

(define (impulse-maker a y)
  (lambda (x)
    (if (= x a)
        y
        0)))

(define my-impulse-function
  (impulse-maker 0 6))

(define (n-fold-smooth f n)
  ((repeated smooth n) f))