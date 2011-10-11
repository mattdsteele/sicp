(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

  (define (same-row? k positions)
    (cond
        ((empty? positions) false)
        ((= (car (car positions)) (car k)) true)
        (else (same-row? k (cdr positions)))))

(define (on-diagonal? k positions)
        (cond
         ((empty? positions) false)
         ((= (abs (- (car k) (car (car positions))))
             (abs (- (cdr k) (cdr (car positions)))))
          true)
         (else (on-diagonal? k (cdr positions)))))

  (define (safe? k positions)
    (not (or (same-row? (car positions) (cdr positions))
             (on-diagonal? (car positions) (cdr positions)))
    ))

(define (adjoin-position row col rest-of-queens)
  (cons (cons row col) rest-of-queens))

(define empty-board '())