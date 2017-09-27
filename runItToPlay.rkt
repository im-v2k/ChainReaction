#lang racket

#|
  The team:
     Aryan        160050053
     Sanchit Jain 160050043
     Varun Kumar  160050084
|#

#|
   The main code starts from here and ends on line number 617
   After that, there is the display routine which has code of start page and end page
   newGame.rkt contains the board, and some very essential functions.
   You will need 'rsound' package to run the game with sound.
   If you couldn't install it in your system, comment out the lines that give error
   i.e., the lines 23, 31, 32 and 135 in this code.
|#

(require racket/gui)
(require graphics/graphics)
(require "coreDeclarations.rkt")
;;;;;; COMMENT OUT THE NEXT LINE IF YOU DON'T HAVE RSOUND PACKAGE
(require rsound)


(provide Alive board currP lm plaY)
(define VOLUME 1.0)
(define FREQUENCY 1000)

;;;;;; COMMENT OUT THE NEXT TWO LINES IF YOU DON'T HAVE RSOUND PACKAGE
(define (sine-tone f)
  (* VOLUME (sin (* 2 pi FREQUENCY (/ f FRAME-RATE)))))


(define frame-size 750)
(define l 50)
(define bitmap-size-x 1920)
(define bitmap-size-y 1080)

(define Alive (list 1 2))  ;It isthe list of surviving players

(define currP 1)  ;It denotes current player


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Following will be used for undo
(define prevboard board)
(define prevPl Alive)
(define prevcurP currP)
(define prevpl (colGen))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (isiPresentInl? i l)
  (if (null? l) #f
      (if (equal? (car l) i) #t
          (isiPresentInl? i (cdr l)))))

(define (update-player-list)
  (define fl (remove 0 (sort (remove-duplicates (map (λ (c) (cell-owner c)) (append* board))) <)))
  (set! Alive fl))

(define allPlayed? #f) ; It becomes true, after each player has made atleast one move

(define (switch-player)  ;It sets the current player to next player
  (define (sph l)
    (cond [(null? l) (set! currP 0)]
          [(and (= (car l) currP) (null? (cdr l))) (set! currP (car Alive))]
          [(= (car l) currP) (set! currP (cadr l))]
          [else (sph (cdr l))]))
  (sph Alive))



(define (list-reader i j)        ; It returns the cell at ith row and jth column
  (cond [(and (> i 0) (> j 0) (<= i NOR) (<= j noc))
         (list-traveller j (list-traveller i board))]
        [else (cell 0 0 0)]))    ; Meaningless output for meaningless input. Fair enough!



(define (list-maker-explodable)               ; It makes a list of lists where each inner
  (define (core r)                            ; list has i j and the cell at i, j, such that, that cell
    (define listtrav (list-traveller r board)); has mass > thr
    (define (innercore s)
      (let ((cel (list-traveller s listtrav)))
        (if (> (cell-mass cel) (cell-thr cel)) (list (list r s cel)) '())))
    (append* (map innercore (itrtr 1 noc))))
  (append* (map core (itrtr 1 NOR))))

(define (exploder i j)  ;It explodes one cell
  (define cel0 (list-reader i j))
  (define decrement 0)
  (cond [(> (- i 1) 0) (set! decrement (+ decrement 1))])
  (cond [(<= (+ i 1) NOR) (set! decrement (+ decrement 1))])
  (cond [(> (- j 1) 0) (set! decrement (+ decrement 1))])
  (cond [(<= (+ j 1) noc) (set! decrement (+ decrement 1))])
  ; Now decrement = (total number of neighbors of i, jth cell)
  ; Neighbors means those cells, which share a common edge with the cell of interest
  (let* [(cel1 (list-reader i (+ j 1)))
         (cel2 (list-reader (- i 1) j))
         (cel3 (list-reader i (- j 1)))
         (cel4 (list-reader (+ i 1) j))]
    (begin (lm i (+ j 1) (cell (cell-owner cel0) (+ (cell-mass cel1) 1) (cell-thr cel1)))
           (lm (- i 1) j (cell (cell-owner cel0) (+ (cell-mass cel2) 1) (cell-thr cel2)))
           (lm i (- j 1) (cell (cell-owner cel0) (+ (cell-mass cel3) 1) (cell-thr cel3)))
           (lm (+ i 1) j (cell (cell-owner cel0) (+ (cell-mass cel4) 1) (cell-thr cel4)))
           (lm i j (cell (if (= (- (cell-mass cel0) decrement) 0) 0 (cell-owner cel0))
                         (- (cell-mass cel0) decrement) (cell-thr cel0))))))

(define (updator) ;It makes a list of explodable cells in the board, and then finally
  (define (uh l)  ;It explodes each of them only ones
    (cond [(null? l) (update-player-list)]
          [else (begin
                  (exploder (caar l) (cadar l))
                  (uh (cdr l)))]))
  (uh (list-maker-explodable)))

(define (ceremony)  ;This function is called when any player wins
  (send msg set-label (string-append  "Player " (number->string (car Alive)) " WINS!"))
  (send dialog show #t))

(define (updataxxxx)  ;Its main function is to call updator until there is no
  (define (uh l)      ;cell, whose mass is greater than threshold.
    (if (null? l)     ;If it finds that only one player is remaining, it calls ceremony.
        (begin        ;This denotes end of the current game.
          (cond [allPlayed? (update-player-list)])
          (if (isSingleton? Alive)
              (begin (set! currP (car Alive)) (ceremony) (disableInp 0)) 
              (begin (switch-player) (disableInp 0) (Display (colGen) (pla-col currP 0)))))
        (begin
          (disableInp 1)
          (ball-burster)
;;;;;; COMMENT OUT THE NEXT LINE IF YOU DON'T HAVE RSOUND PACKAGE
          (play (build-sound 3000 sine-tone))
          (updator)
          (Display (colGen) (pla-col currP 0))
          (cond [allPlayed? (update-player-list)])
          (if (isSingleton? Alive) (ceremony) (uh (list-maker-explodable))))))
  (uh (list-maker-explodable))
  (dynamic-balls))

(define (undo)        
  (cond [(not dispInpDisable?)
         (begin (set! Alive prevPl)
                (board-setter prevboard)
                (set! currP prevcurP)
                (Display prevpl (pla-col currP 0))
                (send frame set-status-text "")
                (disableInp 0))]))

(define (plaY i j) ;It plays (i, j) on the board 
  (if (or (< i 0) (< j 0) (> i NOR) (> j noc))
      (begin 
        (display "i, j should be within proper limits")
        (newline))
      (begin
        (let ((target (list-reader i j)))
          (cond [(or (= (cell-owner target) 0) (= (cell-owner target) currP))
                 (begin
                   (set! prevPl Alive)
                   (set! prevboard board)
                   (set! prevcurP currP)
                   (set! prevpl (colGen))
                   (lm i j (cell currP (+ 1 (cell-mass target)) (cell-thr target)))
                   (if (= (car (reverse Alive)) currP) (set! allPlayed? #t) (set! allPlayed? allPlayed?))
                   (updataxxxx))])))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Frame components

(define origin-x 210)
(define origin-y 120)
(define frame (new frame%
                   [label "Chain Reaction"]
                   [width frame-size]
                   [height frame-size]
                   [x 761]
                   [y 30]
                   [min-width (* l noc)]
                   [min-height (* l NOR)]))

(define dialog (new dialog%
                    [label "Hurray!"]
                    [parent frame]
                    [min-width 400]
                    [min-height 100]
                    [x 800]
                    [y 450]))

(define msg (new message% [parent dialog]
                 [label (string-append  "Player " (number->string (car Alive)) " WINS!")]))

(define topPane (new horizontal-pane% [parent frame] [min-height 50] [stretchable-height #f]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;These are the variables used to call newgame in newgame button
(define propn_o_p 2)
(define propNOR NOR)
(define propnoc noc)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (validtr val idd) ;This function checks if val is in the proper limits
  (cond [(= idd 1)        ;idd tells validtr that val should be checked against
         (cond [(< val 4) ;correct values for NOR, noc, n_o_p
                (begin    
                  (set! propNOR 4)
                  (send tNOR set-value "4"))]
               [(> val 15)
                (begin
                  (set! propNOR 15)
                  (send tNOR set-value "15"))]
               [else (set! propNOR val)])]
        
        [(= idd 2)
         (cond [(< val 4)
                (begin
                  (set! propnoc 4)
                  (send tnoc set-value "4"))]
               [(> val 15)
                (begin
                  (set! propnoc 15)
                  (send tnoc set-value "15"))]
               [else (begin (set! propnoc val))])]
        
        [(= idd 0)
         (if (< val 2)
             (begin
               (set! propn_o_p 2)
               (send tnop set-value "2"))
             (if (> val 8)
                 (begin
                   (set! propn_o_p 8)
                   (send tnop set-value "8"))
                 (set! propn_o_p val)))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Text fields
(define tNOR (new text-field% [parent topPane]
                  [label "Number of rows: "]
                  [init-value (number->string NOR)]))

(define tnoc (new text-field% [parent topPane]
                  [label "Number of columns: "]
                  [init-value (number->string noc)]))

(define tnop (new text-field% [parent topPane]
                  [init-value "2"]
                  [label "Number of players: "]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Buttons and dialog boxes
(define undoButton (new button% [parent topPane]
                        [label "Undo"]
                        
                        (callback (lambda (button event)
                                    (undo)))))

(define (heartOfngButton)
  (let* [(strNOR (send tNOR get-value))
         (strnoc (send tnoc get-value))
         (strnop (send tnop get-value))
         (noNOR (if (eq? strNOR "")
                    10
                    (string->number strNOR)))
         (nonoc (if (eq? strnoc "")
                    10
                    (string->number strnoc)))
         (nonop (if (eq? strnop "")
                    2
                    (string->number strnop)))]
    (begin
      (validtr nonop 0)
      (validtr noNOR 1)
      (validtr nonoc 2)
      (newGame propn_o_p propNOR propnoc))))

(define ngButton           ;newgame button in main window
  (new button% [parent topPane]
       [label "New Game"]
       (callback
        (lambda (button event)
          (cond ((or (not allPlayed?) (and allPlayed? (isSingleton? Alive)))
                 (heartOfngButton))
                (else (send NgConfirmDialog show #t)))))))

(define NgConfirmDialog
  (new dialog%
       [label "Sure about this?"]
       [parent frame]
       [min-width 500]
       [min-height 100]
       [x 800]
       [y 450]))

(define msg2 (new message% [parent NgConfirmDialog]
                  [label "Do you really want to start a new game? "]))

(define backButton          ;back button in newgame confirmation dialog
  (new button% [parent NgConfirmDialog]
       [label "No, take me back"]
       (callback
        (λ (button event)
          (begin (send NgConfirmDialog show #f))))))

(define ngButton2         ;newgame button in newgame confirmation dialog
  (new button% [parent NgConfirmDialog]
       [label "Yes, start a new game"]
       (callback
        (λ (button event)
          (begin (heartOfngButton)
                 (send NgConfirmDialog show #f))))))

(define xButton           ;exit button in main window
  (new button% [parent topPane]
       [label "Exit"]
       (callback
        (lambda (button event)
          (begin (send dialog show #f)
                 (send frame show #f) (exit-screen))))))

(define rtnButton          ;return button in exit dialog
  (new button% [parent dialog]
       [label "Back to game!"]
       [callback
        (λ (button event)
          (begin (send dialog show #f)))]))

(define xButtonD            ;exit button in exit dialog
  (new button% [parent dialog]
       [label  "Exit game"]
       [callback
        (λ (button event)
          (begin (send frame show #f) 
                 (send dialog show #f)(exit-screen)))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (newGame n_o_p NoRR NoCC) ;Creates a new game with n_o_p number of players
  (if (and (> NoRR 3) (> NoCC 3)) ;NoRR number of rows and NoCC number of columns
      (begin (board-changer NoRR NoCC)
             (board-setter board)
             (set! allPlayed? #f)
             (updateBornnCR n_o_p NoRR NoCC)
             (set! Alive Born)
             (set! currP 1)
             (auto-set-origin)
             (set! prevPl Alive)
             (set! prevcurP currP)
             (set! prevboard board)
             (disableInp 0)
             (send frame set-status-text "")
             (Display (colGen) (pla-col currP 0)))
      (begin (string-append "NOR and noc should be greater than 3. i = "
                            (number->string NoRR)
                            " and j = "
                            (number->string NoCC)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Functions used for the display of balls and their animations

(define d 15)   ;d is the diameter of balls
(define color-list (list "red" "green" "royalblue" "yellow" "saddlebrown" "cyan" "magenta" "lightgray"))


(define no-pen (make-object pen% "BLACK" 1 'transparent))

(define (brush i) (make-object brush% (color-fn i) 'solid))

(define (color-fn i)   ;chooses the right color for the ball according to the player
  (list-ref color-list (- i 1)))

(define (my-circle x y d Pl.no)   ;draws a circle at (x,y) with diameter 'd' and color corresponding to the right player
  (send bm-dc set-brush (brush Pl.no))
  (send bm-dc set-pen no-pen)
  (send bm-dc draw-ellipse (- x (/ d 2)) (- y (/ d 2)) d d))

(define 1-ball
  (lambda (x y theta-deg Pl.no)
    (my-circle x y d Pl.no)))

(define 2-balls
  (lambda (x y theta-deg Pl.no)
    (let ((p (/ d 2))
          (theta (* (/ theta-deg 180) pi)))
      (my-circle (+ x (* p (cos theta))) (- y (* p (sin theta))) d Pl.no)
      (my-circle (+ x (* p (cos (+ pi theta)))) (- y (* p (sin (+ pi theta)))) d Pl.no)
      (send canvas refresh))))
  
;sample input : (2-balls (* (/ d 2) (+ (/ 1 (sqrt 2)) 1)) (* (/ d 2) (+ (/ 1 (sqrt 2)) 1)) (/ pi 4) 1)

(define 3-balls
  (lambda (x y theta-deg Pl.no)
    (let ((p (/ d (sqrt 3)))
          (theta (* (/ theta-deg 180) pi)))
      (my-circle (+ x (* p (cos theta))) (- y (* p (sin theta))) d Pl.no)
      (my-circle (+ x (* p (cos (+ (/ (* 2 pi) 3) theta)))) (- y (* p (sin (+ (/ (* 2 pi) 3) theta)))) d Pl.no)
      (my-circle (+ x (* p (cos (+ (/ (* 4 pi) 3) theta)))) (- y (* p (sin (+ (/ (* 4 pi) 3) theta)))) d Pl.no)
      (send canvas refresh))))

;sample-input : (3-balls (* (/ d 2) (+ (/ 1 (sqrt 3)) 1)) d 0 1)


(define (f-balls i)
  (cond [(= i 1) 1-ball]
        [(= i 2) 2-balls]
        [(= i 3) 3-balls]))
;        [(= i 4) 4-balls]))
;;;;;;;;;;;;
;these are state variables used to exit iterator in dynamic-balls
(define XX 0)

(define YY XX)
;;;;;;;;;;;;;;


(define (dynamic-balls)  ;puts all the balls of the board in rotation by repeatedly calling
  (define (iterator i)   ;place-balls with different angles each time, thus giving a rotating effect
    (if (not (= XX YY))
        (Display (colGen) (pla-col currP 0)) 
        (begin (place-balls board i)
               (sleep/yield 0.01) 
               (send bm-dc clear)
               (send bm-dc set-brush "BLACK" 'solid)
               (send bm-dc draw-rectangle (+ origin-x 0) (+ origin-y 0) (* noc l) (* NOR l))
               (draw-grid (pla-col currP 0))
               (send canvas refresh)
               (set! YY XX)
               (if (= i 355)
                   (iterator (/ (- (current-inexact-milliseconds) init-time) 2))
                   (iterator (+ i 5))))))
  (begin 
    (iterator (/ (- (current-inexact-milliseconds) init-time) 2))))

(define (place-balls board angle)  ;places all the balls on an empty grid one-by-one by taking board as an argument
  (let* [(L-x (send canvas get-width))
         (L-y (send canvas get-height))
         (l-x (* l noc))
         (l-y (* l NOR))
         (x-N (/ (- L-x l-x) 2))
         (y-N (/ (- L-y l-y) 2))]        
    (define row-placer (lambda (row row-no i)
                         (cond [(not (null? row))
                                (begin
                                  (cond [(and (> (cell-mass (car row)) 0) (<= (cell-mass (car row)) (cell-thr (car row))))
                                         ((f-balls (cell-mass (car row)))
                                          (+ x-N (/ (* (- (* 2 i) 1) l) 2)) (+ y-N (/ (* (- (* 2 row-no) 1) l) 2)) angle (cell-owner (car row)))]
                                        ;[(> (cell-mass (car row)) (cell-thr (car row)))
                                        ;((f-balls (cell-thr (car row)))
                                        ;(+ x-N (/ (* (- (* 2 i) 1) l) 2)) (+ y-N (/ (* (- (* 2 row-no) 1) l) 2)) angle (cell-owner (car row)))])
                                        )
                                  (row-placer (cdr row) row-no (+ i 1)))])))
    (define column-placer (lambda (board i)
                            (cond [(not (null? board))
                                   (begin (row-placer (car board) i 1)
                                          (column-placer (cdr board) (+ i 1)))])))
    (column-placer board 1)))

(define init-time 0)  ;used to initialize the variable init-time


(define 1-ball-burst 
  (lambda (x y x-dir y-dir percent Pl.no)
    (my-circle (+ x (* x-dir (/ percent 100) l)) (+ y (* y-dir (/ percent 100) l)) d Pl.no)))

(define 2-ball-burst    ;used for animating a situation where a corner cell bursts
  (lambda (x y x-dir y-dir percent Pl.no)
    (1-ball-burst x y x-dir 0 percent Pl.no)
    (1-ball-burst x y 0 y-dir percent Pl.no)))

(define 3-ball-burst    ;used for animating a situation where an edge cell(excluding corner cells) burtsts
  (lambda (x y x-dir y-dir percent Pl.no)
    (cond [(= x-dir 0)
           (begin (1-ball-burst x y 1 0 percent Pl.no)
                  (1-ball-burst x y -1 0 percent Pl.no)
                  (1-ball-burst x y 0 y-dir percent Pl.no))]
          [(= y-dir 0)
           (begin (1-ball-burst x y 0 1 percent Pl.no)
                  (1-ball-burst x y 0 -1 percent Pl.no)
                  (1-ball-burst x y x-dir 0 percent Pl.no))])))

(define 4-ball-burst   ;used for animating a situation where a core cell bursts
  (lambda (x y x-dir y-dir percent Pl.no)
    (1-ball-burst x y 1 0 percent Pl.no)
    (1-ball-burst x y -1 0 percent Pl.no)
    (1-ball-burst x y 0 1 percent Pl.no)
    (1-ball-burst x y 0 -1 percent Pl.no)))

(define (f-burst i) ;returns the required function from the above four functions
  (cond [(= i 0) 1-ball-burst]
        [(= i 1) 2-ball-burst]
        [(= i 2) 3-ball-burst]
        [(= i 3) 4-ball-burst]))

(define (f-dir t lst)  ;used to give directions in which the balls moveafter bursting
  (define x 1)
  (define y 1)
  (cond [(and (= (car lst) 1) (= (cadr lst) 1)) (begin (set! x 1) (set! y 1))]
        [(and (= (car lst) 1) (= (cadr lst) noc)) (begin (set! x -1) (set! y 1))]
        [(and (= (car lst) NOR) (= (cadr lst) 1)) (begin (set! x 1) (set! y -1))]
        [(and (= (car lst) NOR) (= (cadr lst) noc)) (begin (set! x -1) (set! y -1))]
        [(= (car lst) 1) (begin (set! x 0) (set! y 1))]
        [(= (car lst) NOR) (begin (set! x 0) (set! y -1))]
        [(= (cadr lst) 1) (begin (set! x 1) (set! y 0))]
        [(= (cadr lst) noc) (begin (set! x -1) (set! y 0))])
  (if (= t 1) x y))   
         

(define (ball-burster)   ;animated the bursting situation by repeatedly calling one of the above "n-ball-burst' function
  (let* [(L-x (send canvas get-width))                                     ;n = 1, 2, 3, 4
         (L-y (send canvas get-height))
         (l-x (* l noc))
         (l-y (* l NOR))
         (x-N (/ (- L-x l-x) 2))
         (y-N (/ (- L-y l-y) 2))
         (lst (list-maker-explodable))]
    (define (helper percent lst1)
      (cond [(not (null? lst1))
             (begin ((f-burst (cell-thr (caddr (car lst1))))
                     (+ x-N (/ (* (- (* 2 (cadr (car lst1))) 1) l) 2))
                     (+ y-N (/ (* (- (* 2 (car (car lst1))) 1) l) 2))
                     (f-dir 1 (car lst1))
                     (f-dir 2 (car lst1))
                     percent
                     (cell-owner (caddr (car lst1))))                      
                    (helper percent (cdr lst1)))]))
    (define (iterator i)
      (cond [(< i 101)
             (sleep/yield 0.015)
             (send bm-dc clear)
             (draw-grid (pla-col currP 0))               
             (place-balls board (/ (- (current-inexact-milliseconds) init-time) 2))
             (begin (helper i lst) (send canvas refresh) (iterator (+ i 10)))]))
    (iterator 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Frame components again

(define our-canvas%
  (class canvas%
    (define/override (on-event evnt)
      (cond [(and (not dispInpDisable?) (send evnt button-down?))
             (let* ((x (send evnt get-x))
                    (y (send evnt get-y)))
               (begin
                 (plc x y) (set! XX (+ XX 1))))]))
    (super-new)))

(define (plc x y)   ;plc stands for play-controller
  (let* [(L-x (send canvas get-width))
         (L-y (send canvas get-height))
         (l-x (* l noc))
         (l-y (* l NOR))
         (x-shift (/ (- L-x l-x) 2))
         (y-shift (/ (- L-y l-y) 2))
         (x-N (- x x-shift))
         (y-N (- y y-shift))
         (jj (+ (floor (/ x-N l)) 1))
         (ii (+ (floor (/ y-N l)) 1))]
    (begin (cond [(not (or (<= ii 0) (<= jj 0) (> ii NOR) (> jj noc))) (plaY ii jj)]))))

(define canvas
  (new our-canvas% [parent frame]
       [min-width (* l noc)]
       [min-height (* l NOR)]
       [paint-callback
        (lambda (canvas dc) (paint dc))]))

(define (auto-set-origin)    ;sets the origin we defined to the required position in case of a change in canvas size
  (let* [(L-x (send canvas get-width))
         (L-y (send canvas get-height))
         (l-x (* l noc))
         (l-y (* l NOR))
         (x-shift (/ (- L-x l-x) 2))
         (y-shift (/ (- L-y l-y) 2))]
    (begin
      (set! origin-x x-shift)
      (set! origin-y y-shift))))


(define (paint dc) (send dc draw-bitmap face-bitmap 0 0))

(define face-bitmap (make-object bitmap% bitmap-size-x bitmap-size-y ))  ;creates a bitmap

(define bm-dc (make-object bitmap-dc% face-bitmap))  ;creates a drawing context for the bitmap

(send bm-dc clear)

(define (draw-grid colorr)    ;draws the grid on a black background using the sub-functions drhori and drvert that 
  (define grid-pen (make-object pen% colorr 2 'solid))       ;draws horizontal and vertical lines respectively
  (define no-pen (make-object pen% "BLACK" 2 'transparent))
  
  (send bm-dc set-pen no-pen)
  (send bm-dc set-brush "BLACK" 'solid)
  (send bm-dc draw-rectangle 0 0 (send canvas get-width) (send canvas get-height))
  (send bm-dc set-pen grid-pen)
  (define (drhori i)
    (send bm-dc draw-line
          (+ origin-x 0) (+ origin-y (* i l))
          (+ origin-x (* noc l)) (+ origin-y (* i l))))
  (define (drvert j)
    (send bm-dc draw-line
          (+ origin-x (* j l)) (+ origin-y 0)
          (+ origin-x (* j l)) (+ origin-y (* NOR l))))
  (for ([i (itrtr 0 NOR)])
    (drhori i))
  (for ([j (itrtr 0 noc)])
    (drvert j)))

(define dispInpDisable? #f)

(define (disableInp p)
  (if (= p 0) (set! dispInpDisable? #f) (set! dispInpDisable? #t)))

(define (Display pl colorr)  ;updates  the display-state of the game
  (auto-set-origin)
  (send bm-dc clear)
  (draw-grid colorr)
  (define (dispH l)
    (cond [(not (null? l))
           (begin
             (place-balls board (/ (- (current-inexact-milliseconds) init-time) 2))
             (dispH (cdr l)))]))
  (dispH pl)
  (send canvas refresh))


(define (playGame)   ;initiates the gameplay
  (begin (send frame show #t)
         (send frame fullscreen #t)
         (sleep/yield 0.2)
         (set! init-time (current-inexact-milliseconds))
         (send bm-dc set-brush "BLACK" 'solid)
         (send bm-dc draw-rectangle 0 0 (send canvas get-width) (send canvas get-height))
         (Display (colGen) (pla-col currP 0))         
         (send canvas refresh)))


(open-graphics)
(define work-space (open-viewport "Chain-Reaction" 1430 850));1430 * 850 viewport created which is our work-space
((draw-viewport work-space) "black") ; we prefer on working classic color like black
(define (my-draw-line p1 p2)       ; just a simple function to create a green line between poins p1 p2
  ((draw-line work-space) p1 p2 "green")) 

(define (god-work l) ; a function which takes a list of buttons and check for hover and click operations 
  (define list_original l)
  (define (hover l y)  ; checks if mouse is hovering on a button
    (cond [(not (null? l))
           (begin (let* ((button (car l)))
             (cond [(in-region? y (send button region)) (send button hover-draw)]
                   [(not (in-region? y (send button region))) (send button stop-hover-draw)]))
                  (hover (cdr l) y))]))
  (define state 1)
  (define (click l p) ;checks if mouse is clicked or not
    (cond [(and (not (null? l)) (equal? state 1))
           (begin (let* ((button (car l)))
                    (cond [(in-region? p (send button region)) (begin (send button action)
                                                                           (set! state 0))]))
                  (click (cdr l) p))]
          [(and (null? l) (equal? state 1)) (god-work list_original)]))

  (begin (let* ((y (query-mouse-posn work-space)))
             (hover l y))
         (let* ((x (ready-mouse-click work-space)))
             (if (equal? x #f) (god-work l)
                 (let* ((p (mouse-click-posn x)))
                   (click l p))))))



(define (in-region? p reg) ; tells if point p is in region or not
  (let* ((x0 (posn-x p))
         (y0 (posn-y p))
         (xl (car reg))
         (yl (cadr reg))
         (xu (caddr reg))
         (yu (cadddr reg)))
    (and (and (> x0 xl) (< x0 xu)) (and (> y0 yl) (< y0 yu)))))
(define my-button%  ; our wonderful class which implements butons and hovering
  (class object%
    (init-field p)  ;upper left corner of position of button
    (init-field length)
    (init-field height)
    
    (init-field str)   ; name appearing on button
    (init-field function)   ; activity performed if clicked
    (super-new)
    (define/public (region) ; returns region of the button
      (let* ((x0 (posn-x p))
             (y0 (posn-y p)))
        (list x0 y0 (+ x0 length) (+ y0 height))))
    (define/public (action) (function))

    (define/public (hover-draw)
      (let* ((x0 (posn-x p))
             (y0 (posn-y p)))
        ((draw-ellipse work-space) (make-posn (- x0 10) (- y0 5))  (+ length 20) (+ height 10) "green") 
        ))
    (define/public (stop-hover-draw)
      (let* ((x0 (posn-x p))
             (y0 (posn-y p)))
        ((draw-ellipse work-space) (make-posn (- x0 10) (- y0 5))  (+ length 20) (+ height 10) "black") 
        ))

    (define/public (draw)
      (let* ((x0 (posn-x p))
             (y0 (posn-y p))
             (l (string-length str))
             )
        (begin  ((draw-solid-ellipse work-space) p length height "green")
                ((draw-solid-ellipse work-space) (make-posn (+ x0 (* 0.05 length)) (+ y0 (* 0.05 height))) (* 0.9 length) (* 0.9 height) "black"))
        ((clear-string work-space) (make-posn (- (+ x0 (/ length 2)) (* 10 (/ l 2))) (+ y0 (/ height 2) 5))  str)))))


#|
;Beginning of welcome screen
(define (welcome-to-the-world-of-cs154)
  (define delta-time 0.25)
  (define small-time 0.1)
  (define time1 0.2)

  (define (check)
    (let* ((x (ready-mouse-click work-space)))
      (cond [(not (equal? x #f)) (begin (set! delta-time 0)
                                        (set! small-time 0)
                                        (set! time1 0))])))
  (define scale 10)
  (define (cursor p)
    ((draw-solid-rectangle work-space) p (* 3 scale) scale "green"))
  (define (hide-cursor p)
    ((draw-solid-rectangle work-space) p (* 3 scale) scale "black"))
  (define (w p)
    (let* ((x0 (posn-x p))
           (y0 (posn-y p)))
      (cursor (make-posn (+ x0 (* 13 scale)) (+ y0 (* 10 scale))))
      (my-draw-line p (make-posn (+ x0 (* 3 scale)) (+ y0 (* 10 scale))))
      (my-draw-line (make-posn (+ x0 (* 3 scale)) (+ y0 (* 10 scale))) (make-posn (+ x0 (* 6 scale)) (+ y0 (* 6 scale))))
      (my-draw-line (make-posn (+ x0 (* 6 scale)) (+ y0 (* 6 scale))) (make-posn (+ x0 (* 9 scale)) (+ y0 (* 10 scale))))
      (my-draw-line (make-posn (+ x0 (* 9 scale)) (+ y0 (* 10 scale))) (make-posn (+ x0 (* 12 scale)) y0))
      (sleep/yield delta-time)
      (hide-cursor (make-posn (+ x0 (* 13 scale)) (+ y0 (* 10 scale))))))
  (define (e p)
    (let* ((x0 (posn-x p))
           (y0 (posn-y p)))
      (cursor (make-posn (+ x0 (* 4 scale)) (+ y0 (* 6 scale))))
      (my-draw-line p (make-posn (+ x0 (* 3 scale)) y0))
      (my-draw-line (make-posn (+ x0 (* 3 scale)) y0) (make-posn (+ x0 (* 3 scale)) (+ y0 (* 3 scale))))
      (my-draw-line (make-posn (+ x0 (* 3 scale)) (+ y0 (* 3 scale))) (make-posn x0 (+ y0 (* 3 scale))))
      (my-draw-line p (make-posn x0 (+ y0 (* 6 scale))))
      (my-draw-line (make-posn x0 (+ y0 (* 6 scale))) (make-posn (+ x0 (* 3 scale)) (+ y0 (* 6 scale))))
      (sleep/yield delta-time)
      (hide-cursor (make-posn (+ x0 (* 4 scale)) (+ y0 (* 6 scale))))))
  (define (l p)
    (let* ((x0 (posn-x p))
           (y0 (posn-y p)))
      (cursor (make-posn (+ x0 (* 2 scale)) (+ y0 (* 10 scale))))
      (my-draw-line p (make-posn x0 (+ y0 (* 9 scale))))
      (my-draw-line (make-posn x0 (+ y0 (* 9 scale))) (make-posn (- x0 (* 1 scale)) (+ y0 (* 9 scale))))
      (my-draw-line (make-posn (- x0 (* 1 scale)) (+ y0 (* 9 scale))) (make-posn (- x0 (* 1 scale)) (+ y0 (* 10 scale))))
      (my-draw-line (make-posn (- x0 (* 1 scale)) (+ y0 (* 10 scale))) (make-posn (+ x0 (* 1 scale)) (+ y0 (* 10 scale))))
      (my-draw-line (make-posn (+ x0 (* 1 scale)) (+ y0 (* 10 scale))) (make-posn (+ x0 (* 1 scale)) (+ y0 (* 9 scale))))
      (sleep/yield delta-time)
      (hide-cursor (make-posn (+ x0 (* 2 scale)) (+ y0 (* 10 scale))))))
  (define (c p)
    (let* ((x0 (posn-x p))
           (y0 (posn-y p)))
      (cursor (make-posn (+ x0 (* 4 scale)) (+ y0 (* 6 scale))))
      (my-draw-line p (make-posn (+ x0 (* 3 scale)) y0))
      (my-draw-line (make-posn (+ x0 (* 3 scale)) y0) (make-posn (+ x0 (* 3 scale)) (+ y0 (* 1 scale))))
      (my-draw-line p (make-posn x0 (+ y0 (* 6 scale))))
      (my-draw-line (make-posn x0 (+ y0 (* 6 scale))) (make-posn (+ x0 (* 3 scale)) (+ y0 (* 6 scale))))
      (my-draw-line (make-posn (+ x0 (* 3 scale)) (+ y0 (* 6 scale))) (make-posn (+ x0 (* 3 scale)) (+ y0 (* 5 scale))))
      (sleep/yield delta-time)
      (hide-cursor (make-posn (+ x0 (* 4 scale)) (+ y0 (* 6 scale))))))
  (define (o p)
    (let* ((x0 (posn-x p))
           (y0 (posn-y p)))
      (cursor (make-posn (+ x0 (* 4 scale)) (+ y0 (* 6 scale))))
      (my-draw-line p (make-posn (+ x0 (* 3 scale)) y0))
      (my-draw-line (make-posn (+ x0 (* 3 scale)) y0) (make-posn (+ x0 (* 3 scale)) (+ y0 (* 6 scale))))
      (my-draw-line p (make-posn x0 (+ y0 (* 6 scale))))
      (my-draw-line (make-posn x0 (+ y0 (* 6 scale))) (make-posn (+ x0 (* 3 scale)) (+ y0 (* 6 scale))))
      (my-draw-line (make-posn (+ x0 (* 1 scale)) (+ y0 (* 3 scale))) (make-posn (+ x0 (* 2 scale)) (+ y0 (* 3 scale))))
      (sleep/yield delta-time)
      (hide-cursor (make-posn (+ x0 (* 4 scale)) (+ y0 (* 6 scale))))))
  (define (m p)
    (let* ((x0 (posn-x p))
           (y0 (posn-y p)))
      (cursor (make-posn (+ x0 (* 9 scale)) (+ y0 (* 6 scale))))
      (my-draw-line p (make-posn (+ x0 (* 6 scale)) y0))
      (my-draw-line (make-posn (- x0 (* 1 scale)) (+ y0 (* 6 scale))) (make-posn x0 (+ y0 (* 6 scale))))
      (my-draw-line p (make-posn x0 (+ y0 (* 6 scale))))
      (my-draw-line (make-posn (+ x0 (* 2 scale)) y0) (make-posn (+ x0 (* 2 scale)) (+ y0 (* 6 scale))))
      (my-draw-line (make-posn (+ x0 (* 4 scale)) y0) (make-posn (+ x0 (* 4 scale)) (+ y0 (* 6 scale))))
      (my-draw-line (make-posn (+ x0 (* 6 scale)) y0) (make-posn (+ x0 (* 6 scale)) (+ y0 (* 6 scale))))
      (my-draw-line (make-posn (+ x0 (* 6 scale)) (+ y0 (* 6 scale))) (make-posn (+ x0 (* 7 scale)) (+ y0 (* 6 scale))))
      (sleep/yield delta-time)
      (hide-cursor (make-posn (+ x0 (* 9 scale)) (+ y0 (* 6 scale))))))
  (define (space p n)
    (let* ((x0 (posn-x p))
           (y0 (posn-y p)))
      (begin (define single-step (lambda (i) (cond [(not (= i n)) (begin (cursor (make-posn (+ x0 (* i 4 scale)) (+ y0 (* 10 scale))))
                                                                         (sleep/yield small-time)
                                                                         (hide-cursor (make-posn (+ x0 (* i 4 scale)) (+ y0 (* 10 scale))))
                                                                         (single-step (+ i 1)))])))
             (single-step 0))))
      
  (define (t p)
    (let* ((x0 (posn-x p))
           (y0 (posn-y p)))
      (cursor (make-posn (+ x0 (* 7 scale)) (+ y0 (* 10 scale))))
      (my-draw-line p (make-posn (+ x0 (* 6 scale)) y0))
      (my-draw-line (make-posn (+ x0 (* 3 scale)) y0) (make-posn (+ x0 (* 3 scale)) (+ y0 (* 9 scale))))
      (my-draw-line (make-posn (+ x0 (* 3 scale)) (+ y0 (* 9 scale))) (make-posn (+ x0 (* 2 scale)) (+ y0 (* 9 scale))))
      (my-draw-line (make-posn (+ x0 (* 2 scale)) (+ y0 (* 9 scale))) (make-posn (+ x0 (* 2 scale)) (+ y0 (* 10 scale))))
      (my-draw-line (make-posn (+ x0 (* 2 scale)) (+ y0 (* 10 scale))) (make-posn (+ x0 (* 4 scale)) (+ y0 (* 10 scale))))
      (my-draw-line (make-posn (+ x0 (* 4 scale)) (+ y0 (* 10 scale))) (make-posn (+ x0 (* 4 scale)) (+ y0 (* 9 scale))))
      (sleep/yield delta-time)
      (hide-cursor (make-posn (+ x0 (* 7 scale)) (+ y0 (* 10 scale))))))

  (define (h p)
    (let* ((x0 (posn-x p))
           (y0 (posn-y p)))
      (cursor (make-posn (+ x0 (* 7 scale)) (+ y0 (* 10 scale))))
      (my-draw-line p (make-posn (+ x0 (* 6 scale)) y0))
      (my-draw-line (make-posn (+ x0 (* 6 scale)) y0) (make-posn (+ x0 (* 6 scale)) (+ y0 (* 1 scale))))
      (my-draw-line p (make-posn x0 (+ y0 (* 10 scale))))
      (my-draw-line (make-posn x0 (+ y0 (* 4 scale))) (make-posn (+ x0 (* 4 scale)) (+ y0 (* 4 scale))))
      (my-draw-line (make-posn (+ x0 (* 4 scale)) (+ y0 (* 4 scale))) (make-posn (+ x0 (* 4 scale)) (+ y0 (* 10 scale))))
      (my-draw-line (make-posn (+ x0 (* 4 scale)) (+ y0 (* 10 scale))) (make-posn (+ x0 (* 5 scale)) (+ y0 (* 10 scale))))
      (sleep/yield delta-time)
      (hide-cursor (make-posn (+ x0 (* 7 scale)) (+ y0 (* 10 scale))))))

  (define (r p)
    (let* ((x0 (posn-x p))
           (y0 (posn-y p)))
      (cursor (make-posn (+ x0 (* 4 scale)) (+ y0 (* 6 scale))))
      (my-draw-line p (make-posn x0 (+ y0 (* 6 scale))))
      (my-draw-line (make-posn x0 (+ y0 (* 1 scale))) (make-posn (+ x0 (* 3 scale)) (+ y0 (* 1 scale))))
      (my-draw-line (make-posn (+ x0 (* 3 scale)) (+ y0 (* 1 scale))) (make-posn (+ x0 (* 3 scale)) (+ y0 (* 2 scale))))
      (sleep/yield delta-time)
      (hide-cursor (make-posn (+ x0 (* 4 scale)) (+ y0 (* 6 scale))))))


  (define (d p)
    (let* ((x0 (posn-x p))
           (y0 (posn-y p)))
      (cursor (make-posn (+ x0 (* 4 scale)) (+ y0 (* 10 scale))))
      (my-draw-line (make-posn x0 (+ y0 (* 4 scale))) (make-posn x0 (+ y0 (* 10 scale))))
      (my-draw-line (make-posn x0 (+ y0 (* 10 scale))) (make-posn (+ x0 (* 3 scale)) (+ y0 (* 10 scale))))
      (my-draw-line (make-posn (+ x0 (* 3 scale)) (+ y0 (* 10 scale))) (make-posn (+ x0 (* 3 scale)) y0))
      (my-draw-line (make-posn x0 (+ y0 (* 4 scale))) (make-posn (+ x0 (* 3 scale)) (+ y0 (* 4 scale))))
      (sleep/yield delta-time)
      (hide-cursor (make-posn (+ x0 (* 4 scale)) (+ y0 (* 10 scale))))))

  (define (f p)
    (let* ((x0 (posn-x p))
           (y0 (posn-y p)))
      (cursor (make-posn (+ x0 (* 4 scale)) (+ y0 (* 10 scale))))
      (my-draw-line p (make-posn (+ x0 (* 3 scale)) y0))
      (my-draw-line p (make-posn x0 (+ y0 (* 13 scale))))
      (my-draw-line (make-posn x0 (+ y0 (* 13 scale))) (make-posn (- x0 (* 2 scale)) (+ y0 (* 13 scale))))
      (my-draw-line (make-posn (- x0 (* 1 scale)) (+ y0 (* 4 scale))) (make-posn (+ x0 (* 1 scale)) (+ y0 (* 4 scale))))
      (my-draw-line (make-posn (- x0 (* 1 scale)) (+ y0 (* 4 scale))) (make-posn (- x0 (* 1 scale)) (+ y0 (* 5 scale))))
      (my-draw-line (make-posn (+ x0 (* 1 scale)) (+ y0 (* 4 scale))) (make-posn (+ x0 (* 1 scale)) (+ y0 (* 3 scale))))
      (sleep/yield delta-time)
      (hide-cursor (make-posn (+ x0 (* 4 scale)) (+ y0 (* 10 scale))))))
  (define scale2 40)
  (define (big-c p)
    (let* ((x0 (posn-x p))
           (y0 (posn-y p)))
      (cursor (make-posn (+ x0 (* 4 scale2)) (+ y0 (* 6 scale2))))
      (my-draw-line p (make-posn (+ x0 (* 3 scale2)) y0))
      (my-draw-line (make-posn (+ x0 (* 3 scale2)) y0) (make-posn (+ x0 (* 3 scale2)) (+ y0 (* 1 scale2))))
      (my-draw-line p (make-posn x0 (+ y0 (* 6 scale2))))
      (my-draw-line (make-posn x0 (+ y0 (* 6 scale2))) (make-posn (+ x0 (* 3 scale2)) (+ y0 (* 6 scale2))))
      (my-draw-line (make-posn (+ x0 (* 3 scale2)) (+ y0 (* 6 scale2))) (make-posn (+ x0 (* 3 scale2)) (+ y0 (* 5 scale2))))
      (sleep/yield delta-time)
      (hide-cursor (make-posn (+ x0 (* 4 scale2)) (+ y0 (* 6 scale2))))))
  (define (big-s p)
    (let* ((x0 (posn-x p))
           (y0 (posn-y p)))
      (cursor (make-posn (+ x0 (* 4 scale2)) (+ y0 (* 6 scale2))))
      (my-draw-line p (make-posn (+ x0 (* 3 scale2)) y0))
      (my-draw-line (make-posn (+ x0 (* 3 scale2)) y0) (make-posn (+ x0 (* 3 scale2)) (+ y0 (* 1 scale2))))
      (my-draw-line p (make-posn x0 (+ y0 (* 3 scale2))))
      (my-draw-line (make-posn x0 (+ y0 (* 3 scale2))) (make-posn (+ x0 (* 3 scale2)) (+ y0 (* 3 scale2))))
      (my-draw-line (make-posn (+ x0 (* 3 scale2)) (+ y0 (* 3 scale2))) (make-posn (+ x0 (* 3 scale2)) (+ y0 (* 6 scale2))))
      (my-draw-line (make-posn x0 (+ y0 (* 6 scale2))) (make-posn (+ x0 (* 3 scale2)) (+ y0 (* 6 scale2))))
      (my-draw-line (make-posn x0 (+ y0 (* 6 scale2))) (make-posn x0 (+ y0 (* 5 scale2))))
      (sleep/yield delta-time)
      (hide-cursor (make-posn (+ x0 (* 4 scale2)) (+ y0 (* 6 scale2))))))
  (define (one p)
    (let* ((x0 (posn-x p))
           (y0 (posn-y p)))
      (cursor (make-posn (+ x0 (* 1 scale2)) (+ y0 (* 6 scale2))))
      (my-draw-line p (make-posn x0 (+ y0 (* 6 scale2))))
      (sleep/yield delta-time)
      (hide-cursor (make-posn (+ x0 (* 1 scale2)) (+ y0 (* 6 scale2))))))
  (define (five p)
    (let* ((x0 (posn-x p))
           (y0 (posn-y p)))
      (cursor (make-posn (+ x0 (* 4 scale2)) (+ y0 (* 6 scale2))))
      (my-draw-line p (make-posn (+ x0 (* 3 scale2)) y0))
      (my-draw-line p (make-posn x0 (+ y0 (* 3 scale2))))
      (my-draw-line (make-posn x0 (+ y0 (* 3 scale2))) (make-posn (+ x0 (* 3 scale2)) (+ y0 (* 3 scale2))))
      (my-draw-line (make-posn (+ x0 (* 3 scale2)) (+ y0 (* 3 scale2))) (make-posn (+ x0 (* 3 scale2)) (+ y0 (* 6 scale2))))
      (my-draw-line (make-posn x0 (+ y0 (* 6 scale2))) (make-posn (+ x0 (* 3 scale2)) (+ y0 (* 6 scale2))))
      (sleep/yield delta-time)
      (hide-cursor (make-posn (+ x0 (* 4 scale2)) (+ y0 (* 6 scale2))))))
  (define (four p)
    (let* ((x0 (posn-x p))
           (y0 (posn-y p)))
      (cursor (make-posn (+ x0 (* 2 scale2)) (+ y0 (* 6 scale2))))
      (my-draw-line p (make-posn x0 (+ y0 (* 6 scale2))))
      (my-draw-line p (make-posn (- x0 (* 2 scale2)) (+ y0 (* 3.5 scale2))))
      (my-draw-line (make-posn (- x0 (* 2 scale2)) (+ y0 (* 3.5 scale2))) (make-posn (+ x0 (* 1 scale2)) (+ y0 (* 3.5 scale2))))
      (sleep/yield delta-time)
      (hide-cursor (make-posn (+ x0 (* 2 scale2)) (+ y0 (* 6 scale2))))))
  (define (blink p n)
    (let* ((x0 (posn-x p))
           (y0 (posn-y p)))
      (begin (define single-step (lambda (i) (cond [(not (= i n)) (begin (cursor (make-posn (+ x0 (* 4 scale)) (+ y0 (* 10 scale))))
                                                                         (sleep/yield delta-time)
                                                                         (hide-cursor (make-posn (+ x0 (* 4 scale)) (+ y0 (* 10 scale))))
                                                                         (sleep/yield time1)
                                                                         (single-step (+ i 1)))])))
             (single-step 0))))
  
  (w (make-posn 70 100))
  (check)
  (e (make-posn 200 140))
  (check)
  (l (make-posn 250 100))
  (check)
  (c (make-posn 270 140))
  (check)
  (o (make-posn 310 140))
  (check)
  (m (make-posn 360 140))
  (check)
  (e (make-posn 440 140))
  (check)
  (space (make-posn 480 100) 7)
  (check)
  (t (make-posn 680 100))
  (check)
  (o (make-posn 750 140))
  (check)
  (space (make-posn 790 100) 7)
  (check)
  (t (make-posn 1000 100))
  (check)
  (h (make-posn 1070 100))
  (check)
  (e (make-posn 1140 140))
  (check)
  (sleep/yield 0.15)
  (w (make-posn 400 300))
  (check)
  (o (make-posn 530 340))
  (check)
  (r (make-posn 580 340))
  (check)
  (l (make-posn 630 300))
  (check)
  (d (make-posn 660 300))
  (check)
  (space (make-posn 700 300) 5)
  (check)
  (o (make-posn 850 340))
  (check)             
  (f (make-posn 910 300))
  (check)             
  (sleep/yield 0.15)
  (check)             
  (big-c (make-posn 400 500))
  (check)             
  (big-s (make-posn 550 500))
  (check)             
  (one (make-posn 720 500))
  (check)             
  (five (make-posn 740 500))
  (check)             
  (four (make-posn 960 500))
  (check)             
  (blink (make-posn 1010 640) 6)
  (check)             
  ((draw-string work-space) (make-posn 1100 780) "Press any key to continue...." "green")
  (check)             
  (define arbid (get-key-press work-space))
  (home-page))
;end of the opening screen
|#


;start of home page
(define (home-page)
  (open-graphics)
  ((draw-viewport work-space) "black")
  (((draw-pixmap-posn "Images/logo copy.png")	 	 	 
    work-space)	 	 	 	 
   (make-posn 400 50))
  (define play-button (make-object my-button% (make-posn 350 450)  200 100 "PLAY" (lambda() (play-screen))))
  (define instruction-button (make-object my-button% (make-posn 750 450)  200 100 "GUIDELINES" (lambda() (instruction-screen))))
  (define exit-button (make-object my-button% (make-posn 900 700)  100 50 "EXIT" (lambda() (exit-screen))))
  (send play-button draw)
  (send instruction-button draw)
  (send exit-button draw)
  (god-work (list play-button instruction-button exit-button)))
;end of home-screen

;start of instruction screen

(define (instruction-screen)
  ((draw-viewport work-space) "black")
  (((draw-pixmap-posn "Images/GUIDELINES_heading.gif")	 	 
    work-space)	 	 	 	 
   (make-posn 500 25))
  (define back-button (make-object my-button% (make-posn 980 700)  100 50 "BACK" (lambda() (home-page))))
  (send back-button draw)
  (((draw-pixmap-posn "Images/guidelines.jpg")	 	 
    work-space)	 	 	 	 
   (make-posn 180 270))
  (god-work (list back-button)))
;end of instruction screen

;start of ending screen
(define (exit-screen) 
  ((draw-viewport work-space) "black")
  (((draw-pixmap-posn "Images/EXIT.png")	 	 
    work-space)	 	 	 	 
   (make-posn 500 25))
  (define bye-button (make-object my-button% (make-posn 980 700)  100 50 "BYE" (lambda() (close-viewport work-space))))
  (send bye-button draw)
  (define back-button (make-object my-button% (make-posn 300 700)  300 50 "NO, I WANT TO PLAY" (lambda() (home-page))))
  (send back-button draw)
  (((draw-pixmap-posn "Images/exit.jpg")	 	 
    work-space)	 	 	 	 
   (make-posn 180 270))
  (god-work (list bye-button back-button)))
;ending of exit screen

;start of play screen
(define (play-screen)
  (playGame))

;end of play screen


;(welcome-to-the-world-of-cs154)
(home-page)


;(close-graphics)



