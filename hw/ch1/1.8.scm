(define (cube-root x)
  (cube-root-iter (/ x 2.0) x x))

(define (cube-root-iter old-guess guess x)
  (if (good-enough? old-guess guess)
      guess
      (cube-root-iter guess
                      (improve guess x)
                      x)))

(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2.0 guess)) 3.0))

(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

(define (good-enough? old-guess guess)
  (< (/ (abs (- old-guess guess)) guess) 0.0001))
