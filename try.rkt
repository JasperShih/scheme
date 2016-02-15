#lang racket
; kn_pt = known_anwser_point
; kn_in = known_anwser_input
; un_pt = unknown_anwser_point
; un_in = unknown_anwser_input
(define kn_pt (solve kn_in))
(define un_pt (solve un_in))

;1.Consider args of trans
;necessary_args ::= {arg}* ; The necessary information to turn kn_in into un_in
;claim:(trans kn_in necessary_args) => (un_in)
;necessary_args = char <- (car un_in)
;usage: (trans char kn_in) => (un_in)
(define trans
  (lambda (char kn_in)
    (cons char kn_in)))

;2.Bind
;claim: (bind necessary_args kn_pt) => (un_pt)
;= (bind char kn_pt) => (un_pt) ; 帶入一組已知的un_pt, kn_pt, char 則可求得 bind
;= (bind char (solve kn_in)) => (solve un_in) ; bind擁有足夠的資訊to turn kn_in into unin
;usage: (bind char kn_pt) => (un_pt)
(define bind
  (lambda (char kn_pt)
    (cond [(null? kn_pt) (list char)]
          [(<= char [car kn_pt]) (cons char kn_pt)]
          [(> char [car kn_pt]) (cons (car kn_pt) (insert char [cdr kn_pt]))]
          [else (error 'error)]
          )))

;3.Solve
; claim: (solve lst) => lst with ascending order
; usage: (solve '(8 2 5 2 3)) => (2 2 3 5 8)
; n <- (length lst)
; (solve lst) = '() x {bind necessary_args}^(n-1)
(define solve
  (lambda (lst)
    (cond [(null? lst) lst]
          [else (bind (car lst) (solve (cdr lst)))])))




















