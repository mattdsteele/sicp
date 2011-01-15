(load "adv-new.scm")

;;;;;;;;;;;;;;; STDIN CONFIG ;;;;;;;;;;;;;;
(define tty-port (current-input-port))

(define (prompt string)
  (if (eq? (current-input-port) tty-port)
      (begin (display string) (flush))))

(define (prompt-enter)
  (display "Press Enter to Continue")
  (prompt ":")
  (begin (flush) (read-char) 'okay))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STORY BEGINS HERE: ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(read-char)
(define the-sky (instantiate place "Castle in the Sky"))
(define nar1 (instantiate narrator "Narrator" the-sky))

;;;; PLACES ;;;

(define (make-n-give person place thing-name)
  (let ((thing (instantiate thing thing-name)))
    (ask place 'appear thing)
    (ask person 'take thing)))

(define (bake-n-give person place food-name calories)
  (let ((food (instantiate food food-name calories)))
    (ask place 'appear food)
    (ask person 'take food)))

(define soda (instantiate place "Sofa Hall"))
(define bh-office (instantiate place "BH-Office"))
(define lab (instantiate place "Sofa Hall Lab"))

(define-class (blueberry-muffin)
  (parent (food "Blueberry Muffin" 1000)))

(define humbird (instantiate restaurant "Mockingbird Cafe" blueberry-muffin 200))

(define (conn from from-dir to to-dir)
  (can-go from from-dir to)
  (can-go to to-dir from))

(define bh (instantiate person "Brian Harley" bh-office))
(ask bh 'put 'intelligence 9001)
(define cory (instantiate place "Curry Hall"))
(define jail (instantiate place 'jail))

(define min (instantiate person "Max Xu" lab))
(define unix (instantiate computer "UNIX Workstation"))
(ask lab 'appear unix)

(make-n-give min lab "Dell Laptop")

(define matloob (instantiate thief "Michael Matloob" jail))

(conn soda 'east cory 'west)
(conn soda 'west humbird 'east)
(conn soda 'up bh-office 'down)
(conn soda 'down lab 'up)
(conn cory 'up jail 'down)

(define total-right 0)

(define (prob-maker quest tf choice1 choice2 choice3 choice4 answer)
  (lambda ()
    (display (string-append quest "\n"))
    (display (string-append choice1 "\n"))
    (display (string-append choice2 "\n"))
    (if (not tf)
	(begin
	  (display (string-append choice3 "\n"))
	  (display (string-append choice4 "\n"))))
    (let ((result (read)))
      (if (equal? result answer)
	  (begin (display "Very good!\n")
		 (set! total-right (+ total-right 1))
		 #t)
	  (begin (display "Not quite!\n")
		 #f)))))

(define prob1
  (prob-maker "What is the runtime of the procedure member?"
	      #f
	      "A: constant"
	      "B: linear"
	      "C: quadratic"
	      "D: exponential"
	      'b))


(define prob2
  (prob-maker "When trying environmental diagrams, all new environments are created in the current environment"
	      #t
	      "A. True"
	      "B. False"
	      ""
	      ""
	      'b))

(define prob3
  (prob-maker "set-car! and set-cdr! are special forms."
	      true
	      "A. True"
	      "B. False"
	      ""
	      ""
	      'b))

(define prob4
  (prob-maker "A list is: "
	      false
	      "A. Sometimes a pair"
	      "B. Always a pair"
	      "C. Never a pair"
	      "D. a tree"
	      'a))

(define prob5
  (prob-maker "Which of the following expressions will NOT create a new environment?"
	      false
	      "A. (define foo (let ((x 5)) x))"
	      "B. ((lambda (x) x) 5)"
	      "C. (define (foo) (let ((x 5)) x))"
	      "D. (define foo ((lambda () 5)))"
	      'c))

(define prob6
  (prob-maker "In Scheme, all procedures are lambda procedures or primitives."
	      true
	      "A. True"
	      "B. False"
	      ""
	      ""
	      'a))

(for-each (lambda (p)
	    (let ((new-prob
		   (instantiate problem 20)))
	      (ask new-prob 'set-content p)
	      (ask unix 'add-problem new-prob)))
		 (list prob1 prob2 prob3 prob4 prob5 prob6))

(define (looping n)
  (if (= n 0) 'okay
      (begin (newline)
	     (looping (- n 1)))))

(looping 30)
(print "==== Marvelous Misadventure of Michael Matloob: Chapter 2 ====")

(newline)
(newline)

(ask nar1 'say "Fact 1: Tsar Ivan the Terrible cited as inspirations the methods of Michael Matloob")

(prompt-enter)

(ask nar1 'say "Fact 2: Michael Matloob is the reason Chuck Norris decided to learn self-defense")

(prompt-enter)

(ask nar1 'say "Michael Matloob, the terror of civilization, was finally apprehended after scouring the surface of earth for years")

(prompt-enter)

(ask nar1 'say "The judge was merciless and the jury bloodthirsty; Michael Matloob was sentenced to 10 years in a prison so secure that even Matloob gave up trying to break out after the 1st year.")

(prompt-enter)

(ask nar1 'say "After what seemed like an eternity, the day finally came; Matloob walked his first step outside the barbed wires of the prison.")

(ask matloob 'go 'down)

(prompt-enter)

(ask nar1 'say "Matloob immediately scanned the surrounding to see what he can steal. He landed his sight on the nearby Sofa Hall.")

(ask matloob 'go 'west)

(prompt-enter)

(ask matloob 'go 'down)

(ask nar1 'say "But once Matloob reached the Sofa Labs, he could not find any valuable computers to steal.")

(prompt-enter)

(ask nar1 'say "Instead, Matloob found the problems displayed on the computer screens rather intriguing and took a closer look ...")

(prompt-enter)

(define (loop-prob n)
  (if (= n 0) 'okay
      (begin (ask matloob 'do-problem unix)
	     (newline)
	     (loop-prob (- n 1)))))

(loop-prob 6)

(read-char)

(if (< total-right 5)
    (begin
      (ask nar1 'say "Unfortunately, Matloob wasn't able to understand computer science and resumed a life of thuggery")
      (prompt-enter)
      (ask nar1 'say "He was shortly caught stealing from Paul Hilfiger and battered to a bloody pulp")
      (prompt-enter)
      (display "YOU LOSE! Try again")
      (newline)
      (exit)))

(ask nar1 'say
     "Matloob quickly mastered Computer Science. He was even able to use it to get a job.")

(prompt-enter)

(ask matloob 'attempt-promotion)
(ask matloob 'work 50)
(ask matloob 'get-paid)

(prompt-enter)

(ask nar1 'say "With more money in hand than ever, Matloob grew fond of CS and wished to learn more. He asked others where he can gain even greater knowledge.")

(prompt-enter)

(ask min 'say "Alas, if CS thou wisheth to know, to  Brian Harley thou must go. Yet go not lightly as a thrift, for Harley will require a gift.")

(prompt-enter)

(ask matloob 'steal-all min)
(ask matloob 'go 'up)
(ask matloob 'go 'up)

(prompt-enter)

(ask matloob 'offer (car (ask matloob 'possessions)) bh)

(prompt-enter)

(ask matloob 'go 'down)

(ask matloob 'go 'west)

(ask matloob 'buy)

(prompt-enter)

(ask matloob 'go 'east)
(ask matloob 'go 'up)
(ask matloob 'offer (car (filter (lambda (x) (ask x 'food?))
			      (ask matloob 'possessions))) bh)

(ask matloob 'offer (car (ask matloob 'possessions)) bh)

(prompt-enter)

(ask nar1 'say "Matloob prospered under the tutelage of Brian Harley and eventually decided to write a story about his life titled The Marvelous Misadventures of Michael Matloob ...")

(prompt-enter)

(load "story1.scm")

