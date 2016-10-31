#lang racket

(define (number->bintree int) {list int '() '()})

(define (current-element bintree) (first bintree))

(define at-leaf? null?)

(define move-to-left-son second)

(define move-to-right-son third)

(define insert-to-left
  (lambda (int bintree)
    {cond
      [(at-leaf? bintree) "can't insert node"]
      [else {list
              (current-element bintree)
              {list int (move-to-left-son bintree) '()}
              (move-to-right-son bintree)}]}))

(define insert-to-right
  (lambda (int bintree)
    {cond
      [(at-leaf? bintree) "can't insert node"]
      [else {list
              (current-element bintree)
              (move-to-left-son bintree)
              {list int (move-to-right-son bintree) '()}}]}))

;(number->bintree 13)
(define t1
  (insert-to-right 14
    (insert-to-left 12
      (number->bintree 13))))
;t1
;(move-to-left-son t1)
;(current-element (move-to-left-son t1))
;(at-leaf? (move-to-right-son (move-to-left-son t1)))
;(insert-to-left 15 t1)
