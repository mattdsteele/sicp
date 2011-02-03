(define (sqrt-iter old-guess guess x)
  (if (good-enough? old-guess guess)
      guess
      (sqrt-iter guess
                 (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? old-guess guess)
  (< (/ old-guess guess) 1.001))

(define (square x)
  (* x x))

(define (sqrt x)
  (sqrt-iter 1.1 1.0 x))
