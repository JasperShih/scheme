#lang racket

(define number->sequence
  (lambda (int)
    {list int '() '()}))

(define current-element
  (lambda (seq)
    (first seq)))

(define at-left-end?
  (lambda (seq)
    (equal? (second seq) '())))

(define at-right-end?
  (lambda (seq)
    (equal? (third seq) '())))

(define insert-to-left
  (lambda (int seq)
    {list
      (first seq)
      (cons int (second seq))
      (third seq)}))

(define insert-to-right
  (lambda (int seq)
    {list
      (first seq)
      (second seq)
      (cons int (third seq))}))

(define move-to-left
  (lambda (seq)
    (if (at-left-end? seq)
        "fail"
        {list
          (car (second seq))
          (cdr (second seq))
          (cons (first seq) (third seq))})))

(define move-to-right
  (lambda (seq)
    (if (at-right-end? seq)
        "fail"
        {list
          (car (third seq))
          (cons (first seq) (second seq))
          (cdr (third seq))})))

;(number->sequence 7)
;(current-element '(6 (5 4 3 2 1) (7 8 9)))
;(move-to-left '(6 (5 4 3 2 1) (7 8 9)))
;(move-to-left '(6 () (7 8 9)))
;(move-to-right '(6 (5 4 3 2 1) (7 8 9)))
;(move-to-right '(6 (5 4 3 2 1) ()))
;(insert-to-left 13 '(6 (5 4 3 2 1) (7 8 9)))
;(insert-to-right 13 '(6 (5 4 3 2 1) (7 8 9)))
