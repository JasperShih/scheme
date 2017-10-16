
(define first car)
(define second cadr)
(define third caddr)
(define identifier? symbol?)

; spec-scanner: String -> Tokens
(define spec-scanner
  '{[space (whitespace) skip]
    [comment ("%" (arbno (not #\newline))) skip]
    [identifier (letter (arbno (or letter digit))) symbol]
    [number (digit (arbno digit)) number]})

;scan&parse: String -> AST
(define scan&parse
  (sllgen:make-string-parser spec-scanner grammar))

(define exception-langval-extract
    (lambda (type val)
      (eopl:error 'LangVal-extract "Looking for a ~s, found ~s" type val)))

(define exception-apply-env
  (lambda (var)
    (eopl:error 'apply-env "No binding for ~s" var)))

; empty-env: Env
(define empty-env 'empty)

; extend-env: SchemeIdentifier X LangVal X Env -> Env
(define extend-env
  (lambda (var val env)
          (list var val env)))

; apply-env: SchemeIdentifier X Env -> LangVal
(define apply-env
  (lambda (var env)
          {cond
            [(eq? env 'empty) (exception-apply-env var)]
            [(eq? var (first env)) (second env)]
            [else (apply-env var (third env))]}))
