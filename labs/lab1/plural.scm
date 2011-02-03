(define (plural wd)
  (word wd 's))

(define (plural wd)
  (if (ends-with-hard-y? wd)
      (word (bl wd) 'ies)
      (word wd 's)))

(define (ends-with-hard-y? wd)
  (and (equal? (last wd) 'y)
       (not (vowel? (last (butlast wd))))))

(define (vowel? c)
  (member? c '(a e i o u)))
