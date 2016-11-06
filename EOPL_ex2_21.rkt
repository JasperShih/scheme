#lang eopl
(define-datatype environment environment?
  [empty-env]
  [extended-env
    (var symbol?)
    (val number?)
    (env environment?)])

(define apply-env
  (lambda (env search-var)
    {cases environment env
      [extended-env (var val env) (if (eq? search-var var)
                                      val
                                      (apply-env env search-var))]
      [empty-env "not-found"]}))

(define has-binding?
  (lambda (env search-var)
    (if (eq? (apply-env env search-var) "not-found")
        #f
        #t)))
