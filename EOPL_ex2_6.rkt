#lang racket

;;1) 
;;((var_1 val_1) (var_2 val_2) ... (var_n val_n))

(define empty '())

;;extend((var val) Env) > Env
(define extend cons)

;;lookup(var Env) > val
(define lookup
	(lambda (search_var env)
		(cond [(eq? env empty) "Not found"]
			  [(eq? (caar env) search_var) (cadar env)]
			  [else (lookup search_var (cdr env))]
			)))


;(lookup 'c (extend '(b 10) (extend '(a 5) empty)))
;;------------------------------------------------------
;;2)

;;empty_2(var) > String
(define empty_2
	(lambda (search_var)
		"Not found"))

;;extend_2(var val Env) > Env
(define extend_2
	(lambda (saved_var saved_val saved_env)
		(lambda (search_var)
			(cond [(eq? search_var saved_var) saved_val]
				  [else (saved_env search_var)]
				))))


;;((extend_2 'b 10 (extend_2 'a 5 empty_2)) 'a)
;;------------------------------------------------------
;;3)
;;(var_1 val_1 (var_2 val_2 (var_n val_n '())))

(define empty_3 '())

;;extend_3(var val Env) > Env
(define extend_3
	(lambda (var val env)
		(list var val env)))

;;lookup_3(var Env) > val
(define lookup_3
	(lambda (search_var env)
		(cond [(eq? env empty_3) "Not found"]
			  [(eq? (first env) search_var) (second env)]
			  [else (lookup_3 search_var (third env))]
			)))

;;(lookup_3 'a (extend_3 'b 10 (extend_3 'a 5 empty_3)))











