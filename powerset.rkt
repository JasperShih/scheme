#lang racket
     
;(define
 ; (powerset lst)
  ;(cond
   ; [(empty? lst) '(())]
    ;[else (opt2
     ;      (opt (car lst) (power (cdr lst)))
      ;     (power (cdr lst)))]))

(define res '())
(define
  (pow raw leaf)
  (cond
    [(empty? raw) (set! res (cons leaf res))
                  res]
    [else (pow (cdr raw) leaf)
          (pow (cdr raw) (cons (car raw) leaf))]))
(pow '(4 5 6 7) '())