#lang eopl

(define-datatype pre-exp pre-exp?
  [const-exp
    (num integer?)]
  [diff-exp
    (operand1 pre-exp?)
    (operand2 pre-exp?)])

;concrete->concrete-with-parentheses
(define add-parentheses
  (lambda (lst)
    (let {[continue #f]}
      [define pass
        (lambda (lst)
          {cond
            [(null? lst) '()]
            [else {case (car lst)
                    [(-) [define operand1 (cadr lst)]
                         [define operand2 (caddr lst)]
                         (if (and
                               (not (eq? operand1 '-))
                               (not (eq? operand2 '-)))
                             (begin
                               (set! continue #t)
                               (cons
                                 (list '- operand1 operand2)
                                 (pass (cdddr lst))))
                             (cons
                               (car lst)
                               (pass (cdr lst))))]
                    [else (cons
                            (car lst)
                            (pass (cdr lst)))]}]})]
      [define new-lst (pass lst)]
      (if continue
          (add-parentheses new-lst)
          new-lst))))

(define parser
  (lambda (exp)
    {cond
      [(null? exp) '()]
      [(number? exp) (list 'const-exp exp)]
      [else (list 'diff-exp (parser (cadr exp)) (parser (caddr exp)))]}))

(define concrete->abstract
  (lambda (lst)
    [define with-parentheses (add-parentheses lst)]
    (parser (car with-parentheses))))

(display (concrete->abstract '(- - 3 2 - 4 - 12 7)))

;--------------------------------------
;抽取一個有用的資訊, 和餘下其他未處理字符串
;(extract chaos-data)
;return (a-known , residual-chaos-data)
(define extract
  (lambda (chaos-data)
    {cond
      [(null? chaos-data) '(empty ())]
      [else [define stuff (car chaos-data)]
            {cond
              [(number? stuff) (list (list 'const-exp stuff) (cdr chaos-data))]
              [else [define next-extract (extract (cdr chaos-data))]
                    [define next-2-extract (extract (second next-extract))]
                    (list
                      (list 'diff-exp (car next-extract) (car next-2-extract))
                      (second next-2-extract))]}]}))


(display (extract '(- - 3 2 - 4 - 12 7)))
