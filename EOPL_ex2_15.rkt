#lang racket
(define var-exp
  (lambda (var)
    var))

(define lambda-exp
  (lambda (bound-var lc-exp)
    {list 'lambda (list bound-var) lc-exp}))

(define app-exp
  (lambda (lc-exp-1 lc-exp-2)
    {list lc-exp-1 lc-exp-2}))

(define var-exp? symbol?)

(define (lambda-exp? lc-exp)
  (and
    (list? lc-exp)
    (eq? (first lc-exp) 'lambda)))

(define (app-exp? lc-exp)
  (and
    (list? lc-exp)
    (not (eq? (first lc-exp) 'lambda))))

(define (var-exp->var var-exp) var-exp)

(define (lambda-exp->bound-var lambda-exp) (first (second lambda-exp)))

(define (lambda-exp->body lambda-exp) (third lambda-exp))

(define (app-exp->rator app-exp) (first app-exp))

(define (app-exp->rand app-exp) (second app-exp))
