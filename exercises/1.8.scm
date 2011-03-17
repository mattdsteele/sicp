(define (sqrt-iter guess prev-guess x)
  (if (good-enough? guess prev-guess)
      guess
      (sqrt-iter (improve guess x) guess
                 x)))

(define (sqrt x)
  (sqrt-iter 1.0 0.0 x))

(define (good-enough? guess prev-guess)
  (< (/ (abs (- prev-guess guess)) guess) .0001))

(define (square x) (* x x))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))