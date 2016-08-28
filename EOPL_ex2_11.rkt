#lang racket

;; 我覺得用2個(或以上)的回傳值, 就能夠解決null的問題,
;; 第二個值是實際回傳值, 第一個值是描述第二個值,
;; (null,)表null, 而(non_null, 10)表示有東西, 
;; (non_null, null)表示null這個東西


(define atom?
  (lambda (x)
    (and (not (pair? x))
    	 (not (null? x))
         )
    ))

;;car是(vars vals)pair, 而cadr是saved_env
(define empty '())

;;empty_env?(Env) > Boolean
(define empty_env?
  (lambda (env)
    (eq? env empty)))

;;extend(var val Env) > Env
(define extend
  (lambda (var val env)
    (list (list (list var) 
    	        (list val)) 
    	  env)))

;;multi_extend(Lst Lst Env) > Env
(define multi_extend
  (lambda (vars vals env)
    (list (list vars vals) env)))

;;lookup(var Env) > val
(define lookup
	(lambda (search_var env)
		{cond 
			[(empty_env? env) "Not found"]
			[else (define saved_vars (first (first env)))
				  (define saved_vals (second (first env)))
				  (define saved_env (second env))
				  ;;match_vars(Lst Lst) > val
				  (define match_vars
				  	(lambda (saved_vars saved_vals)
				  		{cond 
				  			[(null? saved_vars) (lookup search_var saved_env)]
				  			[(eq? (first saved_vars) search_var) (first saved_vals)]
				  			[else (match_vars (cdr saved_vars) (cdr saved_vals))]
				  			}))
				  (match_vars saved_vars saved_vals)
				  ]}
		))

;;has_binding?(var Env) > Boolean
(define has_binding?
	(lambda (search_var env)
		(cond 
			[(empty_env? env) #f]
			[else (define saved_vars (first (first env)))
				  (define saved_vals (second (first env)))
				  (define saved_env (second env))
				  ;;match_vars(Lst Lst) > Boolean
				  (define match_vars
				  	(lambda (saved_vars saved_vals)
				  		{cond 
				  			[(null? saved_vars) (has_binding? search_var saved_env)]
				  			[else (or 
				  					(eq? (first saved_vars) search_var)
				  				 	(match_vars (cdr saved_vars) (cdr saved_vals)))]}))
				  (match_vars saved_vars saved_vals)]
			)))

;;has_binding會這麼複雜, 我認為是設計錯誤造成的
;;有atom save_vars,又有list save_vars
;;is should be ((vars vals) Env)

;;正常寫出來的單元程式, 縮進套嵌應該不會太深, 層次架構應不至於太複雜,
;;如果不是這樣, 很有可能是程式設計錯誤了, 如同這個例子一樣.
(has_binding? 'g (multi_extend '(a b c) '(10 11 12) (extend 'e 20 (extend 'd 15 empty))))