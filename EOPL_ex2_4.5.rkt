#lang racket

;; data types:
;; Env: empty_env | (extend_env var val Env)

;;empty_env() >Env
(define empty_env '(empty_env))

;;extend_env(var val Env) > Env
(define extend_env
	(lambda (var val env)
		(list 'extended_env var val env)))

;;env_lookup(var Env) > val

(define env_lookup
	(lambda (search_var env)
		(cond [(eq? (car env) 'empty_env) "The var is not found"]
			  [else [define saved_var (second env)]
			  		[define saved_val (third env)]
			  		[define saved_env (fourth env)]
			  		(cond [(eq? search_var saved_var) saved_val]
			  			  [else (env_lookup search_var saved_env)]
			  			)
			  		]
			)))


(env_lookup 'c (extend_env 'b 20 
			     (extend_env 'a 10 empty_env))
)