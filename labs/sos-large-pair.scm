(define (sos-large-pair a b c)
  (cond ((and (< a b) (< a c)) (sos b c))
	((and (< b a) (< b c)) (sos a c))
	(else (sos a b))))


(define (sos a b)
  (+ (square a) (square b)))

(define (square a)
  (* a a))
