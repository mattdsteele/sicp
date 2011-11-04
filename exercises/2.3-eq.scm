(define (memeq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memeq item (cdr x)))))