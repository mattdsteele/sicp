;; ADV.SCM
;; This file contains the definitions for the objects in the adventure
;; game and some utility procedures.

(load "tables.scm")

(define-class (place name)
  (instance-vars
   (directions-and-neighbors '())
   (things '())
   (people '())
   (entry-procs '())
   (exit-procs '()))
  (method (type) 'place)
  (method (neighbors) (map cdr directions-and-neighbors))
  (method (exits) (map car directions-and-neighbors))
  (method (look-in direction)
    (let ((pair (assoc direction directions-and-neighbors)))
      (if (not pair)
	  '()                     ;; nothing in that direction
	  (cdr pair))))           ;; return the place object
  (method (appear new-thing)
    (if (memq new-thing things)
	(error "Thing already in this place" (list name new-thing)))
    (set! things (cons new-thing things))
    'appeared)
  (method (enter new-person)
    (if (memq new-person people)
	(error "Person already in this place" (list name new-person)))
    (set! people (cons new-person people))
    (for-each (lambda (proc) (proc)) entry-procs)
    'appeared)
  (method (gone thing)
    (if (not (memq thing things))
	(error "Disappearing thing not here" (list name thing)))
    (set! things (delete thing things)) 
    'disappeared)
  (method (exit person)
    (for-each (lambda (proc) (proc)) exit-procs)
    (if (not (memq person people))
	(error "Disappearing person not here" (list name person)))
    (set! people (delete person people)) 
    'disappeared)

  (method (new-neighbor direction neighbor)
    (if (assoc direction directions-and-neighbors)
	(error "Direction already assigned a neighbor" (list name direction)))
    (set! directions-and-neighbors
	  (cons (cons direction neighbor) directions-and-neighbors))
    'connected)

  (method (add-entry-procedure proc)
    (set! entry-procs (cons proc entry-procs)))
  (method (add-exit-procedure proc)
    (set! exit-procs (cons proc exit-procs)))
  (method (remove-entry-procedure proc)
    (set! entry-procs (delete proc entry-procs)))
  (method (remove-exit-procedure proc)
    (set! exit-procs (delete proc exit-procs)))
  (method (clear-all-procs)
    (set! exit-procs '())
    (set! entry-procs '())
    'cleared) )

(define-class (person name place)
  (instance-vars
   (possessions '())
   (saying "Hello!"))
  (initialize
   (ask place 'enter self))
  (method (type) 'person)
  (method (look-around)
    (map (lambda (obj) (ask obj 'name))
	 (filter (lambda (thing) (not (eq? thing self)))
		 (append (ask place 'things) (ask place 'people)))))

   (method (take thing)
    (cond ((not (thing? thing)) (error "Not a thing" thing))
	  ((not (memq thing (ask place 'things)))
	   (error "Thing taken not at this place"
		  (list (ask place 'name) thing)))
	  ((memq thing possessions) (error "You already have it!"))

	  ;; Make sure no one else has it already
	  ((not (possessed? thing))
	       (announce-take name thing)
	       (set! possessions (cons thing possessions))
	       (ask thing 'change-possessor self))
	  (else
	       (error "Someone else has it already!"))))
	   
  (method (lose thing)
    (set! possessions (delete thing possessions))
    (ask thing 'change-possessor 'no-one)
    'lost)
  (method (say stuff)
	  (display name) (display ": ")
	  (print stuff))
	  
  (method (talk) (ask self 'say saying))
  (method (set-talk string) (set! saying string))
  (method (exits) (ask place 'exits))
  (method (notice person) (ask self 'talk))
  (method (go direction)
    (let ((new-place (ask place 'look-in direction)))
      (cond ((null? new-place)
	     (error "Can't go" direction))
	    (else
	     (ask place 'exit self)
	     (announce-move name place new-place)
	     (for-each
	      (lambda (p)
		(ask place 'gone p)
		(ask new-place 'appear p))
	      possessions)
	     (set! place new-place)
	     (ask new-place 'enter self)
	     

	     )))) )

(define-class (thing name)
  (instance-vars (possessor 'no-one))
  (method (type) 'thing)
  (method (change-possessor new-possessor)
	  (set! possessor new-possessor)))

(define-class (narrator name initial-place)
  (parent (person name initial-place)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation of Food  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FILL IN


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Implementation of Thief and Police
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class (thief name initial-place)
  (parent (person name initial-place))
  (method (type) 'thief)
  (method (steal new-thing)
	  ;; FILL IN
	  )
  (method (steal-all new-person)
	  (cond ((not (memq new-person (ask (ask self 'place) 'people)))
		 (error "Person not at this place"))
		(else
		 (display name)
		 (display " is stealing from ")
		 (display (ask new-person 'name))
		 (newline)
		 (for-each (lambda (t)
			     (ask self 'steal t))
			   (ask new-person 'possessions))))))

(define jail (instantiate place 'jail))

;; Implementation of Police
;; FILL IN


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (possessed? thing)
  (not (equal? (ask thing 'possessor) 'no-one)))
	      
;;; this next procedure is useful for moving around

(define (move-loop who)
  (newline)
  (print (ask who 'exits))
  (display "?  > ")
  (let ((dir (read)))
    (if (equal? dir 'stop)
	(newline)
	(begin (print (ask who 'go dir))
	       (move-loop who)))))


;; One-way paths connect individual places.

(define (can-go from direction to)
  (ask from 'new-neighbor direction to))


(define (announce-take name thing)
  (newline)
  (display name)
  (display " took ")
  (display (ask thing 'name))
  (newline))

(define (announce-move name old-place new-place)
  (newline)
  (newline)
  (display name)
  (display " moved from ")
  (display (ask old-place 'name))
  (display " to ")
  (display (ask new-place 'name))
  (newline))

(define (have-fit p)
  (newline)
  (display "Yaaah! ")
  (display (ask p 'name))
  (display " is upset!")
  (newline))

(define (pick-random set)
  (nth (random (length set)) set))

(define (delete thing stuff)
  (cond ((null? stuff) '())
	((eq? thing (car stuff)) (cdr stuff))
	(else (cons (car stuff) (delete thing (cdr stuff)))) ))

(define (person? obj)
  (and (procedure? obj)
       (member? (ask obj 'type) '(person police thief))))

(define (thing? obj)
  (and (procedure? obj)
       (eq? (ask obj 'type) 'thing)))
