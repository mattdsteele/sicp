(load "adv.scm")

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

(define telegraph (instantiate place "Phonograph Ave."))
(define matloob (instantiate thief "Michael Matloob" telegraph))
(define hippie (instantiate person "Hippie" telegraph))
(make-n-give hippie telegraph "Tie-Dye Tshirt")

(define soda (instantiate place "Sofa Hall"))
(define bh (instantiate person "Brian Harley" soda))
(bake-n-give bh soda "Potsticker" 100)

(define cory (instantiate place "Curry Hall"))
(define babak (instantiate person "Babak Azayifar" cory))
(make-n-give babak cory "Barbecue Grill")

(define south-ag (instantiate place "South Food Court"))
(define student1 (instantiate person "Starving Student" south-ag))
(bake-n-give student1 south-ag "Blueberry Muffin" 300)

(define sproul (instantiate place "Sprout Plaza"))
(define admin (instantiate person "Administrator" sproul))
(make-n-give admin sproul "Scholarship Money")
(define hhh (instantiate person "Happy-Happy-Happy Man" telegraph))
(make-n-give hhh telegraph "Happiness")
(ask hhh 'set-talk "Happy! Happy! Happy!")

(define glade (instantiate place "Memorial Knoll"))
(define player (instantiate person "Ultimate Player" glade))
(make-n-give player glade "Frisbee Disk")

(define leconte (instantiate place "LaConte Hall"))
(define smoot (instantiate person "George Snoot" leconte))
(make-n-give smoot leconte "Nobel Prize Medal")

(define foothill (instantiate place "Footmill Dorm"))
(define student2 (instantiate person "Engineering Student" foothill))
(make-n-give student2 foothill "Homework")
(bake-n-give student2 foothill "Ramen" 150)
(make-n-give student2 foothill "Structure and Interpretation of Computer Programs")

(define stanford (instantiate place "Stanfurd University"))
(define richstudent (instantiate person "Rich Student" stanford))
(make-n-give richstudent stanford "Gold-plated Private Jet")

(define haas (instantiate place "Haas School of Nonsense"))
(define student3 (instantiate person "Business Major" haas))
(make-n-give student3 haas "Large Bag of Unmarked Bills")

(define greek (instantiate place "Roman Theatre"))
(define student4 (instantiate person "Graduating Senior" greek))
(make-n-give student4 greek "Diploma")

(define ihouse (instantiate place "Intercontinental House"))
(define student5 (instantiate person "Foreign Student" ihouse))
(make-n-give student5 ihouse "Passport")

(define (conn from from-dir to to-dir)
    (can-go from from-dir to)
    (can-go to to-dir from))

(conn telegraph 'east south-ag 'west)
(conn telegraph 'north sproul 'south)
(conn sproul 'north glade 'south)
(conn sproul 'east haas 'west)
(conn glade 'east leconte 'west)
(conn leconte 'north soda 'south)
(conn soda 'east cory 'west)
(conn cory 'east foothill 'west)
(conn foothill 'south greek 'north)
(conn greek 'south ihouse 'north)
(conn ihouse 'west south-ag 'east)
(conn telegraph 'south stanford 'north)

;;;; STORY ;;;;;

(define (looping n)
  (if (= n 0) 'okay
      (begin (newline)
	     (looping (- n 1)))))

(looping 30)
(print "===== Marvelous Misadventure of Michael Matloob: Chapter 1 ======")
(newline)
(newline)
(ask nar1 'say "This is the marvelous, magnificent misadventure of Michael Matloob, of a miscreant turned master, of a man turned myth. They say he was a mad monster, they say he was a mystical magician, they say he was a misunderstood mortal; but who is he really? Let's find out ...")

(prompt-enter)

(ask nar1 'say "Michael Matloob began his legendary life as a street urchin on Phonograph Avenue. He did not seem extraordinary at the time. He was short, thin; he had unassuming manners and small eyes that raised no eyebrows of passer-bys. But Matloob has a sharp mind and he used it to quickly become the master thief of Phonograph Avenue.")

(prompt-enter)

(ask nar1 'say "Matloob started out stealing small things.")
(prompt-enter)

(ask hippie 'say "My tie-dye shirt sure looks nice!")
(ask matloob 'steal-all hippie)

(prompt-enter)

(ask matloob 'go 'east)
(ask matloob 'steal-all student1)

(ask matloob 'eat)

(prompt-enter)
 
(ask nar1 'say "Over time and with experience, Matloob grew more brazen")

(prompt-enter)

(ask matloob 'go 'west)
(ask matloob 'go 'north)
(ask matloob 'steal-all admin)

(prompt-enter)

(ask nar1 'say "The Morality of Matloob's actions often escaped him")

(prompt-enter)

(ask hhh 'go 'north)

(ask matloob 'steal-all hhh)

(prompt-enter)

(ask matloob 'go 'east)
(ask matloob 'steal-all student3)

(prompt-enter)

(ask matloob 'go 'west)
(ask matloob 'go 'north)

(ask matloob 'steal-all player)

(prompt-enter)

(ask matloob 'go 'east)
(ask matloob 'steal-all smoot)

(prompt-enter)

(ask matloob 'go 'north)
(ask matloob 'steal-all bh)

(ask matloob 'eat)

(prompt-enter)

(ask matloob 'go 'east)
(ask matloob 'steal-all babak)

(prompt-enter)

(ask matloob 'go 'east)
(ask matloob 'steal-all student2)

(ask matloob 'eat)

(prompt-enter)

(ask matloob 'go 'south)
(ask matloob 'steal-all student4)

(prompt-enter)
(ask matloob 'go 'south)
(ask matloob 'steal-all student5)

(prompt-enter)

(ask matloob 'go 'west)
(ask matloob 'go 'west)

(prompt-enter)

(ask nar1 'say "Though he accumulated great riches, Matloob was not content with just stealing from areas around Phonograph Ave.")

(prompt-enter)

(ask matloob 'go 'south)

(ask matloob 'steal-all richstudent)

(ask matloob 'go 'north)

(prompt-enter)

(ask nar1 'say "But Matloob's exploits did not go unnoticed. The police department dispatched two of its best agents: Chris Lim and Charles Chan, with the specific mission of hunting down Matloob.")

(define chris (instantiate police "Chris Lim" soda))
(define charles (instantiate police "Charles Chan" greek))

(prompt-enter)

(define (arrest-matloob)
	(ask matloob 'randomly-move 1)
	(ask charles 'randomly-move 3)
	(ask chris 'randomly-move 3)
	(if (or (eq? (ask (ask matloob 'place) 'name) 'jail)
		(null? (ask (ask matloob 'place) 'exits)))
	    (begin
	      (ask nar1 'say "On a fateful afternoon, Matloob's luck finally ran out")
	      (prompt-enter))
	    (begin (ask nar1 'say "Matloob is not an easy catch. His great stealth and vigilance allowed him to elude capture for one more day")	
		   (prompt-enter)
		   (arrest-matloob))))

(arrest-matloob)

(ask nar1 'say "Matloob was cornered and arrested. His stolen goods returned and he himself jailed for 10 years.")

(prompt-enter)

(ask nar1 'say "And so for 10 years Matloob sat in the dark and dank corner of his prison cell. Constantly thinking... Plotting ....")

(prompt-enter)

(display "=== END of Chapter I ===")
