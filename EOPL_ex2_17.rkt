#lang racket

;1)
;lc-exp::= (id identifier)
;      ::= (lambda (identifier) lc-exp)
;      ::= (app lc-exp lc-exp)
(define var-exp
  (lambda (var)
    (list 'id var)))

(define lambda-exp
  (lambda (bound-var lc-exp)
    {list 'lambda bound-var lc-exp}))

(define app-exp
  (lambda (lc-exp-1 lc-exp-2)
    {list 'app lc-exp-1 lc-exp-2}))

(define (var-exp? lc-exp) (eq? (first lc-exp) 'id))

(define (lambda-exp? lc-exp)
  (eq? (first lc-exp) 'lambda))

(define (app-exp? lc-exp)
  (eq? (first lc-exp) 'app))

(define (var-exp->var var-exp) (second var-exp))

(define (lambda-exp->bound-var lambda-exp) (second lambda-exp))

(define (lambda-exp->body lambda-exp) (third lambda-exp))

(define (app-exp->rator app-exp) (second app-exp))

(define (app-exp->rand app-exp) (third app-exp))

;-----------------------------------------------------------------
;2)
;lc-exp::= identifier
;      ::= (identifier lc-exp)
;      ::= (app lc-exp-1 lc-exp-2)

;identifier can't be app

(define var-exp
  (lambda (var)
    var))

(define lambda-exp
  (lambda (bound-var lc-exp)
    {list bound-var lc-exp}))

(define app-exp
  (lambda (lc-exp-1 lc-exp-2)
    {list 'app lc-exp-1 lc-exp-2}))

(define var-exp? symbol?)

(define (lambda-exp? lc-exp)
  (and
    (list? lc-exp)
    (not (eq? (first lc-exp) 'app))))

(define (app-exp? lc-exp)
  (and
    (list? lc-exp)
    (eq? (first lc-exp) 'app)))

(define (var-exp->var var-exp) var-exp)

(define (lambda-exp->bound-var lambda-exp) (first lambda-exp))

(define (lambda-exp->body lambda-exp) (second lambda-exp))

(define (app-exp->rator app-exp) (second app-exp))

(define (app-exp->rand app-exp) (third app-exp))
