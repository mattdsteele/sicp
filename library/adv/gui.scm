;;;;;;;;;;;;;;;;;;;LOAD FILES
(load "obj.scm")
(load "tables.scm")

(load "adv.scm")
;;;;;;;;;;;;;;;;;; END LOAD FILES

(define (filter pred? L)
  (cond ((null? L) '())
	((pred? (car L)) (cons (car L) (filter pred? (cdr L))))
	(else (filter pred? (cdr L)))))

;;;;;;;;;;;;;;;;;;;;;;;; SET UP A ROOM ;;;;;;;;;;;
(define soda (instantiate place 'Soda-hall))
(define min (instantiate person 'min soda))
(define matloob (instantiate person 'matloob soda))
(define charles (instantiate person 'charles soda))
(define chris (instantiate person 'chris soda))
(Define bm (instantiate thing 'muffin))
(ask soda 'appear bm)

;; Min's new places
(define cory (instantiate place 'cory-hall))
(define george (instantiate person 'george cory))
(can-go soda 'north cory)
(can-go cory 'south soda)

;;;;;;;;;;;;END OF ROOM SETUP;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;; PEOPLE/PLACES/THINGS MANAGEMENT ;;;;;;;

(define place-assoc '())
(define people-assoc '())
(define thing-assoc '())

(define current-place #f)
(define current-place-gui #f)
(define current-person #f)
(define direct-object-person #f)
(define current-thing #f)

(define people-on-screen '())

;;;;;;;;;;;;;;;END of MANAGEMENT ;;;;;;;;;;;;;;;;;

(define (remove item L)
  (cond ((null? L) '())
	((equal? item (car L)) (remove item (cdr L)))
	(else (cons item (remove item (Cdr L))))))

(define-class (canvas-manager c)
  (instance-vars (image-ids '()) (groups '()))
  (method (register-item item group)
    (set! image-ids (cons item image-ids)))
  (method (delete-all-items)
     (for-each (lambda (x) (c 'delete x)) image-ids)
     (set! groups '())
     (set! image-ids '()))
  (method (delete-group g)
     (if (assoc g groups)
	 (let ((GA (assoc g groups)))
	   (for-each (lambda (x)
		       (if (member? x image-ids)
			   (begin
			     (set! image-ids (remove x image-ids))
			     (c 'delete 'x))))
		     (cdr GA))
	   (set! groups (remove GA groups)))))
  (method (draw-item-with-caption x y picture caption click-action group)
    (define tpict (c 'create 'image x y :image picture :anchor 'n))
    (ask self 'register-item tpict group)
    (c 'bind tpict "<1>" click-action)
    (ask self 'register-item
	 (c 'create 'text x (+ y  (image 'height picture) 5)
		  :text caption)
	 group)
    (image 'width picture)))

(define (get-person-image p)
  .personpict)

(define-class (place-gui place-class image screen)
  (method (draw-list L x y picture proc)
     (define (helper curx cury items)
       (cond ((null? items) 'done)
	     (else (helper (+  5 curx
			      (ask screen 'draw-item-with-caption
				   curx
				   cury 
				   picture
				   (symbol->string
				    (ask (car items) 'name))
				   (proc (car items))
				   #f))
			   cury
			   (cdr items)))))
     (helper x y L))
  (method (draw-people)
     (ask self 'draw-list (ask place-class 'people) 50 50 .personpict
	  (lambda (P) (lambda () (set! current-person P) (redisplay)  ))))
  (method (draw-things)
     (ask self 'draw-list (filter
			   (lambda (t) (eq? (ask t 'possessor) 'no-one))
			   (ask place-class 'things))
	  50 200 .thingpict
	  (lambda (T) (lambda () (if current-person
				     (Begin (ask current-person 'take T)
					    (print (ask T 'possessor))
					    (redisplay))))))))
	  
(define-class (person-gui person-class image)
  (method (do-nothing) #f))

(define (set-place! the-place)
  (let ((ass-p-gui (assoc the-place place-assoc)))
    (set! current-place the-place)
    (if ass-p-gui
	(begin
	  (set! current-place-gui (cdr ass-p-gui))
	  (if (ask current-place-gui 'image)
	      #f ;; SHOULD BE CODE FOR BACKGROUND
	      ))
	(begin
	  (set! current-place-gui (instantiate place-gui the-place #f the-screen))
	  (set! place-assoc (cons (cons current-place current-place-gui)
				  place-assoc))))
    (redisplay)))

(define (redisplay)
;;  (print "redisplaying")
  (ask the-screen 'delete-all-items)
  (.main.screen 'itemconfigure place-title :text
	   (symbol->string (ask current-place 'name)))
  (.main.screen 'itemconfigure item-title :text
	   (if current-person
	       (ask current-person 'name)
	       "No person selected"))
  (ask current-place-gui 'draw-people)
  (ask current-place-gui 'draw-things))
;;;;;;;;;;;;;;;;SET UP TK SCREEN;;;;;;;;;;;;;;;
(frame '.main)

(define height 480)
(define width 640)
(define x (/ width 2) )
(define y (/ height 2))
(canvas '.main.screen :height height :width 640)
;;(pack .screen)

(define the-screen (instantiate canvas-manager .main.screen))

;; Testing Grid <min>

(frame '.main.f :borderwidth 2 :relief 'sunken :width 200 :height 100)
(button '.main.north :text 'North)
(button '.main.south :text 'South)
(button '.main.west :text 'West)
(button '.main.east :text 'East)
(button '.main.reload :text 'Reload)
(button '.main.up :text 'Up)
(button '.main.down :text 'Down)

(grid '.main :column 0 :row 0)
(grid '.main.screen :column 1 :row 1)
(grid '.main.north :column 1 :row 0)
(grid '.main.reload :column 2 :row 0)
(grid '.main.south :column 1 :row 2)
(grid '.main.west :column 0 :row 1)
(grid '.main.east :column 2 :row 1)
(grid '.main.up :column 0 :row 2)
(grid '.main.down :column 2 :row 2)

(define back (.main.screen 'create 'polygon 0 0 0 height width height width 0 :fill ""))
(.main.screen 'lower back)
(.main.screen 'bind back "<1>" (lambda () (set! current-person #f) (redisplay)))
	      
(define (gui-dir-proc DIR)
  (lambda (|A|)
    (print (string-append "camera-" (symbol->string DIR)))
    (let ((new-place (ask current-place 'look-in DIR)))
      (cond ((null? new-place) (print (string-append "ERROR no "
						     (symbol->string DIR))))
	    (current-person (ask current-person 'go DIR)
			    (set! current-person #f)
			    (redisplay))
	    (else (set! current-person #f) (set-place! new-place)  )))))

(bind '.main.reload "<1>" (lambda () (redisplay)))

(bind '.main.up "<1>" (gui-dir-proc 'up))
(bind '.main.down "<1>" (gui-dir-proc 'down))

(bind '.main.north "<1>" (gui-dir-proc 'north))
(bind '.main.south "<1>" (gui-dir-proc 'south))
(bind '.main.east "<1>" (gui-dir-proc 'east))
(bind '.main.west "<1>" (gui-dir-proc 'west))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(image 'create 'photo '.personpict :file "person.gif")
(image 'create 'photo '.thingpict :file "bm.gif")

(define place-title (.main.screen 'create 'text (/ width 2) 10  :text "A blueberry muffin"))

(define item-title (.main.screen 'create 'text (/ width 4) 10 :text "the item"))
(define people (.main.screen 'create 'text 2 40 :text "People" :anchor 'w))
(define things (.main.screen 'create 'text 2 190 :text "Things" :anchor 'w))

(define background-image (.main.screen 'create 'image (/ width 2) (/ height 2)))

;; (bind '. "<Down>" (lambda (|A|)
;; 		   ; (.screen 'itemconfigure x :text "Down")))
;; 		    (set! y (+ y 1))
;; 		    (redisplay)))

(set-place! soda)
(pack .main)
