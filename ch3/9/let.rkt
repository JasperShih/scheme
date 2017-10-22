#lang eopl
(require racket/include)
(include "facility.rkt")

; Program ::= Expression
(define-datatype Program Program?
  [pgm-exp (expression Expression?)])

; Expression ::= Number
;            ::= Identifier
;            ::= zero? (Expression)
;            ::= -(Expression, Expression)
;            ::= if Expression then Expression else Expression
;            ::= let Identifier = Expression in Expression
;            ::= minus(Expression)
;            ::= +(Expression, Expression)
;            ::= *(Expression, Expression)
;            ::= /(Expression, Expression)
;            ::= equal?(Expression, Expression)
;            ::= greater?(Expression, Expression)
;            ::= less?(Expression, Expression)
;            ::= emptylist
;            ::= null?
;            ::= cons
;            ::= car
;            ::= cdr
(define-datatype Expression Expression?
  [exp-const (num number?)]
  [exp-var (var identifier?)]
  [exp-zero (expression Expression?)]
  [exp-diff (minuend Expression?) (subtrahend Expression?)]
  [exp-if (pred Expression?) (flow1 Expression?) (flow2 Expression?)]
  [exp-let (var identifier?) (val Expression?) (exp-sub Expression?)]
  [exp-minus (expression Expression?)]
  [exp-add (exp1 Expression?) (exp2 Expression?)]
  [exp-multiply (exp1 Expression?) (exp2 Expression?)]
  [exp-quotient (exp1 Expression?) (exp2 Expression?)]
  [exp-equal? (exp1 Expression?) (exp2 Expression?)]
  [exp-greater? (exp1 Expression?) (exp2 Expression?)]
  [exp-less? (exp1 Expression?) (exp2 Expression?)]
  [exp-list-empty]
  [exp-null? (expression Expression?)]
  [exp-cons (exp1 Expression?) (exp2 Expression?)]
  [exp-car (expression Expression?)]
  [exp-cdr (expression Expression?)])

; The order DO IMPACT result!
; grammar: Tokens -> AST
(define grammar
  '{[Program (Expression) pgm-exp]
    [Expression (number) exp-const]
    [Expression (identifier) exp-var]
    [Expression ("zero?" "(" Expression ")") exp-zero]
    [Expression ("-" "(" Expression "," Expression ")") exp-diff]
    [Expression ("if" Expression "then" Expression "else" Expression) exp-if]
    [Expression ("let" identifier "=" Expression "in" Expression) exp-let] ; TODO multiple set
    [Expression ("minus" "(" Expression ")") exp-minus]
    [Expression ("+" "(" Expression "," Expression ")") exp-add]
    [Expression ("*" "(" Expression "," Expression ")") exp-multiply]
    [Expression ("/" "(" Expression "," Expression ")") exp-quotient]
    [Expression ("equal?" "(" Expression "," Expression ")") exp-equal?]
    [Expression ("greater?" "(" Expression "," Expression ")") exp-greater?]
    [Expression ("less?" "(" Expression "," Expression ")") exp-less?]
    [Expression ("emptylist") exp-list-empty]
    [Expression ("null?" "(" Expression ")") exp-null?]
    [Expression ("cons" "(" Expression "," Expression ")") exp-cons]
    [Expression ("car" "(" Expression ")") exp-car]
    [Expression ("cdr" "(" Expression ")") exp-cdr]})

; LangVal ::= Number
;         ::= Boolean
(define-datatype LangVal LangVal?
  [val-num (num number?)]
  [val-bool (bool boolean?)])

; LangNum->SchemeNum: LangVal -> SchemeVal
(define LangNum->SchemeNum
  (lambda (val)
          {cases LangVal val
            ; SchemeNum -> SchemeNum
            [val-num (num) num]
            [else (exception-langval-extract 'num val)]}))

; LangBool->SchemeBool: LangVal -> SchemeVal
(define LangBool->SchemeBool
  (lambda (val)
          {cases LangVal val
            ; SchemeBool -> SchemeBool
            [val-bool (bool) bool]
            [else (exception-langval-extract 'bool val)]}))

; run: String -> LangVal
(define run
  (lambda (str)
          (value-of-program (scan&parse str))))

; value-of-program: Program -> LangVal
(define value-of-program
  (lambda (pgm)
          {cases Program pgm
            ; Expression -> LangVal
            [pgm-exp (program) (value-of program init-env)]}))

; value-of: Expression X Env -> LangVal
(define value-of
  (lambda (_exp env)
          {cases Expression _exp
            ; SchemeNum -> LangNum
            [exp-const (num) (val-num num)]
            ; SchemeIdentifier -> LangVal
            [exp-var (var) (apply-env var env)]
            ; Expression -> LangBool
            [exp-zero (expression) {cond
                                    [(zero? (LangNum->SchemeNum (value-of expression env))) (val-bool #t)]
                                    [else (val-bool #f)]}]
            ; Expression X Expression -> LangNum
            [exp-diff (minuend subtrahend) (val-num (- (LangNum->SchemeNum (value-of minuend env))
                                                       (LangNum->SchemeNum (value-of subtrahend env))))]
            ; Expression X Expression X Expression -> LangVal
            [exp-if (pred flow1 flow2) {cond
                                         [(LangBool->SchemeBool (value-of pred env)) (value-of flow1 env)]
                                         [else (value-of flow2 env)]}]
            ; SchemeIdentifier X Expression X Expression -> LangVal
            [exp-let (var val exp-sub) (value-of exp-sub (extend-env var (value-of val env) env))]
            ; Expression -> LangNum
            [exp-minus (expression) (val-num (- (LangNum->SchemeNum (value-of expression env))))]
            ; Expression X Expression -> LangNum
            [exp-add (exp1 exp2) (val-num (+ (LangNum->SchemeNum (value-of exp1 env))
                                             (LangNum->SchemeNum (value-of exp2 env))))]
            ; Expression X Expression -> LangNum
            [exp-multiply (exp1 exp2) (val-num (* (LangNum->SchemeNum (value-of exp1 env))
                                                  (LangNum->SchemeNum (value-of exp2 env))))]
            ; Expression X Expression -> LangNum
            [exp-quotient (exp1 exp2) (val-num (quotient (LangNum->SchemeNum (value-of exp1 env))
                                                         (LangNum->SchemeNum (value-of exp2 env))))]
            ; Expression X Expression -> LangBool
            [exp-equal? (exp1 exp2) (value-of ; another approach
                                     (exp-zero (exp-const (LangNum->SchemeNum (value-of (exp-diff exp1 exp2)
                                                                                        env))))
                                     env)]
            ; Expression X Expression -> LangBool
            [exp-greater? (exp1 exp2) (val-bool (> (LangNum->SchemeNum (value-of exp1 env))
                                                   (LangNum->SchemeNum (value-of exp2 env))))]
            ; Expression X Expression -> LangBool
            [exp-less? (exp1 exp2) (val-bool (< (LangNum->SchemeNum (value-of exp1 env))
                                                (LangNum->SchemeNum (value-of exp2 env))))]
            ; () -> LangList; Let LangList use same form as SchemeList?
            [exp-list-empty () '()]
            ; Expression -> LangBool
            [exp-null? (expression) (equal? expression (exp-list-empty))]
            ; Expression X Expression -> LangList
            [exp-cons (exp1 exp2) (cons (value-of exp1 env)
                                        (value-of exp2 env))]
            ; LangList -> LangVal
            [exp-car (expression) (car (value-of expression env))]
            ; LangList -> LangList
            [exp-cdr (expression) (cdr (value-of expression env))]}))


; If we want put init-env before definition of val-num,
; we can use lambda form(lazy evaluation).
; init-env: Env
(define init-env (extend-env 'i (val-num 1)
                   (extend-env 'v (val-num 5)
                     (extend-env 'x (val-num 10) empty-env))))

(define str "let x = 4
             in cons(x,
                     cons(cons(-(x, 1),
                               emptylist),
                          emptylist))")
(display (run str))
;(display (equal? (exp-list-empty) 10))
