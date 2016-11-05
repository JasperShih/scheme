#lang racket

;for a data structure,
;it should be consider 4 functions:
;1) car: show
;2) cdr!: forward data structure
;3) anti-cdr!: resume cdr
;4) cons!: add
;5) anti-cons!: delete
;set! means remain same the type after call the function

(define build-tree-box
  (lambda (num)
    (let {[tree (list '() num '())] [box '()]}
      (lambda (cmd . args)
        {case cmd
          [(tree) tree]
          [(box) box]
          [(cart) (second tree)]
          [(const-left!) (define left-son (car tree))
                         (define middle (second tree))
                         (define right-son (third tree))
                         (set! tree (list (list left-son (car args) '())
                                          middle
                                          right-son))]
          [(const-right!) (define left-son (car tree))
                          (define middle (second tree))
                          (define right-son (third tree))
                          (set! tree (list left-son
                                           middle
                                           (list right-son (car args) '())))]
          [(anti-const-left!) (set! tree (cons '() (cdr tree)))]
          [(anti-const-right!) (define left-son (car tree))
                               (define middle (second tree))
                               (set! tree (list left-son middle '()))]
          [(cdrt-left!) (set! box (cons (cdr tree) box))
                        (set! tree (first tree))]
          [(cdrt-right!) (define tree-abandon (list (first tree) (second tree)))
                         (set! box (cons tree-abandon box))
                         (set! tree (last tree))]
          [(anti-cdr!) (define tree-complete (car box))
                       (set! box (cdr box))
                       (define stuff-1 (first tree-complete))
                       (define stuff-2 (second tree-complete))
                       (if (number? stuff-1)
                           (set! tree (list tree stuff-1 stuff-2))
                           (set! tree (list stuff-1 stuff-2 tree)))]
          [(at-leaf?) (null? tree)]
          [(at-root?) (null? box)]
          [else "OPPS"]}))))

(define tree-1 (build-tree-box 13))
(tree-1 'tree)
(tree-1 'const-left! 100)
(tree-1 'tree)
(tree-1 'anti-const-left!)
(tree-1 'tree)
