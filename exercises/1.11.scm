(define (f x)
  (if (< x 3)
      x
      (+ (f (- x 1)) (* 2 (f (- x 2))) (* 3(f (- x 3))))))

(define (new-f x)
  (if (< x 3)
      x
      (f-iter x 3 2 1 0)))

(define (f-iter x count f1 f2 f3)
  (if (> count x) f1
      (f-iter x
              (+ count 1)
              (+ (* 3 f3) (* 2 f2) f1)
              f1
              f2)))