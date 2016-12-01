#lang racket
;I think normal program is programming according to data structure,
;and this is programming according to (program as data structure).
;
;At beginning, I think list is simplest data structrue.
;Afterward, I think list is stack.
;Now, I consider list represent simplest evaluation, sequential evaluation.

;或許所有args求值都有順序的, 而非parallel, 只是一般的function不能改變args求值順序,
;而lazy evaluation可以args arbitrary求值.

;list代表線性(sequential)
;imperative編程, 指令是線性(sequential)的, 這點上和list沒有什麼差別
;imperative instructs也可視為一個data structure
;list改可以backward的方法是否可以用在imperative instructs上?

;1)normal form
(define search
  (lambda (target lst)
    {cond
      [(null? lst) #f]
      [(eq? target (car lst)) #t]
      [else (search target (cdr lst))]}))

;2-1)until form
;用macro應該就不用用lambda封裝args了
;(or
;  (eq? 'e 'a)
;  (eq? 'e 'b)
;  (eq? 'e 'c)
;  (eq? 'e 'd))

(define build-until-type
  (lambda (type-pred?)
    (lambda (lead-lexp . next-lexp)
      (let {[lead (lead-lexp)]}
        {cond
          [(type-pred? lead) lead]
          [else ((car next-lexp))]}))))

(define until-#t
  (build-until-type (lambda (lead) lead)))

(define lst-eq?
  (lambda (lst)
    {cond
      [(null? lst) #f]
      [else (until-#t
              (lambda () (element-eq? (car lst)))
              (lambda () (lst-eq? (cdr lst))))]}))

(define element-eq?
  (lambda (sym)
    (eq? 'b sym)))

(lst-eq? '(a b c d))

;2-2)until-ref-prior form
(define build-ref-prior-until-type
  (lambda (type-pred?)
    (lambda (lead-lexp . next-lexp)
      (let {[lead (lead-lexp)]}
        {cond
          [(type-pred? lead) lead]
          [else ((car next-lexp) lead)]}))))

(define ref-prior-until-#t
  (build-ref-prior-until-type (lambda (lead) lead)))

;很明顯lst-eq-ref-prior可以用curry或let()把prior抽象出來,
;因為我們可能還沒規劃出最好的形式，所以先不做過早最佳化
(define lst-eq-ref-prior?
  (lambda (lst prior)
    {cond
      [(null? lst) #f]
      [else (ref-prior-until-#t
              (lambda () (element-eq? (car lst) prior))
              (lambda (prior) (lst-eq-ref-prior? (cdr lst) prior)))]}))

(define element-eq?
  (lambda (sym prior)
    (eq? 'e sym)))
(lst-eq-ref-prior? '(a b c d) #f)

;2-3)until variant form
;(define build-next-if-type
;  (lambda (type-pred)
;    (lambda (lead-lexp . next-lexp)
;      (let {[lead (lead-lexp)]}
;        {cond
;          [(type-pred lead) ((car next-lexp))]
;          [else lead]}))))
;
;(define next-if-#f
;  (build-next-if-type (lambda (lead) (not lead))))

;TODO)
先用array改sequential until
再用array實作next-if-ref-prior
再實現next-if-type(可backward的sequential evaluation),(純藉由lambda實現, 或array輔助)