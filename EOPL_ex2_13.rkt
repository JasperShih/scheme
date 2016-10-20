#lang racket
;@obj沒有內建selector未必不好
;@試著不要用bag as obj member(先收集資料,再處理的方法)
;@一般表示法簡單很多, 也沒有比較差

是否要把empty_env和extend_env的search都提出, 合成一個?
如果不把empty_env寫成尾巴特例形式,會有empty_env?如何判斷的問題
對比之前的寫法

;尾巴都是特例, 不用考慮一般公式
;這次實作empty_env as function是為了使用多個define表達式,
;empty_env內部的members實現,都是用function實作,藉由function把members保護起來
;才不會程式一load時就在執行了, 而是call時才執行.
(define empty_env
  (lambda ()
    (define search (lambda (search_var) '(#t 'NOT_FOUND)))
    (define empty_env? (lambda () #t))
    {list
      search
      empty_env?}))

(define car_search first)

(define car_empty_env? second)

(define extend_env
  (lambda (env var val)
    (define search (lambda (search_var)
                     {cond
                       [(eq? search_var var) {list #f val}]
                       [else ([car_search env] search_var)]}))
    (define empty_env? (lambda () #f))
    {list
      search
      empty_env?}))



;([car_search (empty_env)] 'a)
(define stuff (extend_env (extend_env (empty_env) 'a 10) 'b 20))
([car_search stuff] 'b)


;(define empty_env
;  (lambda (selector)
;    {selector
;      search
;      empty_env?}))
;([env pick_search] var)
;([car_search env] search_var)
;我覺得第二種直觀, 第一種不直觀應該是env吃掉pick_search後,
;而pick_search又跑到前面去了(若用obj內建selector寫法, selector是在options list前面)
;不能反序或許這對可讀性是很重要的. 而不是傳遞function as arg影響可讀性?



;(env.search var) oop風格, 沒打算花很多力氣模仿成這樣
;env.empty?
;env.empty?()


;-----------------------new approch-----------------------------
(define build_env
  (lambda (search empty_env?)
    {list
      search
      empty_env?}))

;其實let的[empty_env?_tail #t]可以省略
(define build_empty_env
  (lambda ()
    [define search_tail 'search_tail]
    [define empty_env?_tail #t]
    (build_env
      (build_search search_tail)
      (build_empty_env? empty_env?_tail))))

;最後一行會有效能問題,最好把它變成const,而非每次都要re_build一次
(define build_extended_env
  (lambda (env var val)
    (build_env
      (build_search (list env var val))
      (build_empty_env? #f))))

(define build_empty_env?
  (lambda (bool)
    (lambda () bool)))

(define build_search
  (lambda (receipt)
    [lambda (search_var)
      {cond
        ((eq? receipt 'search_tail) [define not_found '(#t)]
                                    not_found)
        (else [define saved_env (first receipt)]
              [define saved_var (second receipt)]
              [define saved_val (third receipt)]
              [define search_at_saved_env (lambda () ((car_search saved_env) search_var))]
              {cond
                ((eq? search_var saved_var) (list #f saved_val))
                (else (search_at_saved_env))})}]))

;(define build_search
;  (lambda (receipt)
;    [if (eq? receipt 'search_tail)
;        (lambda (search_var) '(#t))
;        (lambda (search_var) (if (eq? search_var (second receipt))
;                                 (three receipt)
;                                 ({car_search (first receipt)} search_var)))]))

(define car_search first)

(define car_empty_env? second)



(define an_env (build_extended_env (build_extended_env (build_extended_env (build_empty_env) 'a 10) 'b 15) 'c 20))
([car_empty_env? an_env])
([car_search an_env] 'c)
