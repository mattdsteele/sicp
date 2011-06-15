(define (iterative-improve good-enough? improve)
  (iterative-improve-iter good-enough? 1 improve))

(define (iterative-improve-iter good-enough? guess improve)
  (if (good-enough? guess)
       guess
       (iterative-improve-iter good-enough? (improve guess))))