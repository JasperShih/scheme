#lang racket

;; 我覺得用2個(或以上)的回傳值, 就能夠解決null的問題,
;; 第二個值是實際回傳值, 第一個值是描述第二個值,
;; (null,)表null, 而(non_null, 10)表示有東西, 
;; (non_null, null)表示null這個東西


(define empty '())

;;extend(var val Env) > Env
(define extend
	(lambda (var val env)
		(list (list var val) env)))

;;lookup(var Env) > val
(define lookup
	(lambda (search_var env)
		(cond [(eq? env empty) "Not_found"]
			  [(eq? (caar env) search_var) (cadar env)]
			  [else (lookup search_var (second env))]
			)
		))

(extend 'a 10 empty)
;;'((a 10) ())

(extend 'b 20 (extend 'a 10 empty))
;;'((b 20) ((a 10) ()))

;;car是(var val)pair, 而cadr是saved_env

