(define (1+ a)
  (+ 1 a))

(define (square a)
  (* a a))

(define (identity a)
  a)

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term
              (next a)
              next
              b))))

(define (sum-int a b)
  (sum identity a 1+ b))

(define (sum-sq a b)
  (sum square a 1+ b))
