#lang racket
(require racket/gui)
(provide (struct-out cell) itrtr lis-crtr board NOR noc Born isSingleton? list-traveller
         pla-col updateBorn updateBornnCR board-changer colGen lm board-setter)

(define NOR 10)
(define noc 10)

(struct cell (owner mass thr) #:transparent)

(define (itrtr lo hi)
  (if (> lo hi) '()
      (cons lo (itrtr (+ lo 1) hi))))

(define (list-traveller i l)     ;list-traveller returns ith element of list l
  (if (< (length l) i) '()
      (if (null? l) (begin (display "NULL list here. i = ") (display i) (newline))
          (if (= i 1) (car l)
              (list-traveller (- i 1) (cdr l))))))

(define (lis-crtr R c)
  (define (outer-core i)
    (cond [(or (= i 1) (= i R)) (map (λ (j)
                                         (if (or (= j 1) (= j c))
                                             (cell 0 0 1)
                                             (cell 0 0 2)))
                                       (itrtr 1 c))]
          [else (map (λ (j) (if (or (= j 1) (= j c))
                                (cell 0 0 2)
                                (cell 0 0 3)))
                     (itrtr 1 c))]))
  (map outer-core (itrtr 1 R)))

(define board (lis-crtr NOR noc))

(define (board-setter bird)
  (set! board bird))

(define (board-changer r c)
  (set! board (lis-crtr r c)))
(define Born (list 1 2))
(define (updateBornnCR n rr cc) (begin
                               (set! NOR rr)
                               (set! noc cc)
                               (set! Born (itrtr 1 n))))
(define (updateBorn n) (set! Born (itrtr 1 n)))

(define (lm i j p)               ; it sets (i,j)th element of board to p
  (define (core r)
    (if (not (= r i)) (list-traveller r board)
        (let ((listtrav (list-traveller r board)))
          (map (λ (s) (if (not (= s j)) (list-traveller s listtrav) p))
               (itrtr 1 noc)))))
  (cond [(and (> i 0) (> j 0) (<= i NOR) (<= j noc))
         (board-setter (map core (itrtr 1 NOR)))]))

(define (isSingleton? l)
  (if (and (not (null? l)) (null? (cdr l))) #t #f))

(define (colGen) (itrtr 1 (* NOR noc)))

(define (pla-col pl req)
  (list-ref (list "red" "green" "royalblue" "yellow" "saddlebrown" "cyan" "magenta" "lightgray") (- pl 1)))
  