#lang racket

;; 我覺得用2個(或以上)的回傳值, 就能夠解決null的問題,
;; 第二個值是實際回傳值, 第一個值是描述第二個值,
;; (null,)表null, 而(non_null, 10)表示有東西, 
;; (non_null, null)表示null這個東西


;;car是(var val)pair, 而cadr是saved_env
(define empty '())

;;empty_env?(Env) > Boolean
(define empty_env?
	(lambda (env)
		(eq? env empty)))

;;extend(var val Env) > Env
(define extend
	(lambda (var val env)
		(list (list var val) env)))

;;multi_extend(Lst Lst Env) > Env
(define multi_extend
	(lambda (vars vals env)
		(cond [(null? vars) env]
			  [else (multi_extend (cdr vars) (cdr vals)
			  	                  (extend (first vars) (first vals) env))]
			)))

;;lookup(var Env) > val
(define lookup
	(lambda (search_var env)
		(cond [(empty_env? env) "Not_found"]
			  [(eq? (caar env) search_var) (cadar env)]
			  [else (lookup search_var (second env))]
			)
		))

;;has_binding?(var Env) > Boolean
(define has_binding?
	(lambda (search_var env)
		(cond [(empty_env? env) #f]
			  [else (or (eq? (caar env) search_var)
			  			(has_binding? search_var (second env)))]
			)))

(lookup 's (multi_extend '(a b c) '(10 11 12) empty))




