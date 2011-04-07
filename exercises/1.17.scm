(define (* a b)
  (cond
   ((= b 1) a)
   ((even? b) (double (* a (halve b))))
   (else (+ a (* a (- b 1))))))

(define (double x) (+ x x))
(define (halve x) (/ x 2))
(define (even? x) (= 0 (remainder x 2)))