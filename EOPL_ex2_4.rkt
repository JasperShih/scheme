#lang racket

(define empty_stack null)

(define top car)

(define pop cdr)

(define push cons)

(define is_empty_stack? null?)

;>empty_stack
;'()

;>(push 'z (push 'b (push 'a empty_stack)))
;'(z b a)

;>(define my_stack (push 'z (push 'b (push 'a empty_stack))))
;>(top my_stack)
;'z

;>(pop my_stack)
;'(b a)

;>(top (pop (pop my_stack)))
;'a

;;List is stack!
;;List processing is stack processing!