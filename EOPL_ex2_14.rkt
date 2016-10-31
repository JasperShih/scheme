#lang racket
;從抽象角度來看,把search等提出合併成一個是否好?

;a0:a4|a1 a2 (a3|a1) ->
;a0:a4|a1 a2 a3|a1
(define empty-env ;a0
  (let {}
    [define search ;a1
      (let {[not-found '(#t)]}
        (lambda (search_var) not-found))]
    [let
      {[search search]}
      [define empty-env? ;a2
        (lambda () #t)]
      [define has-binding? ;a3
        (lambda (test-var)
          (let {[not-found '(#t)]}
            (if (equal? (search test-var) not-found)
                #f
                #t)))]
      [let {}
        (lambda (command . args) ;a4
          (case command
            [(search) (search (first args))]
            [(empty-env?) (empty-env?)]
            [(has-binding?) (has-binding? (first args))]))]]))

(define env-extend
  (lambda (saved-env saved-var saved-val)
    (let {}
      [define search
        (lambda (search-var)
          (if (eq? search-var saved-var)
              saved-val
              (saved-env 'search search-var)))]
      [let
        {[search search]}
        [define empty-env?
          (lambda () #f)]
        [define has-binding?
          (lambda (test-var)
            (let {[not-found '(#t)]}
              (if (equal? (search test-var) not-found)
                  #f
                  #t)))]
        [let {}
          (lambda (command . args)
            (case command
              [(search) (search (first args))]
              [(empty-env?) (empty-env?)]
              [(has-binding?) (has-binding? (first args))]))]])))
