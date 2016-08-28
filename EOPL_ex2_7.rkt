#lang racket

(define empty_env
	(list 'empty_env))

(define extend_env
	(lambda (var val env)
		(list 'extend_env var val env)))

(define apply_env
	(lambda (env search_var)
		{cond
			[(eq? (first env) 'empty_env) (report_no_binding_found search_var env)]
			[(eq? (first env) 'extend_env) (let ([saved_var (second env)]
												 [saved_val (third env)]
												 [saved_env (fourth env)])
												(cond 
													[(eq? saved_var search_var) saved_val]
													[else (apply_env saved_env search_var)]))]
			[else (report_invalid_env env)]
			}))

(define report_no_binding_found
	(lambda (search_var env)
		(display "error: apply_env no binding ")
		(display search_var)
		(display " in ")
		(display env)
		(newline)
		))

(define report_invalid_env
	(lambda (env)
		(display "error: apply_env bad environment ")
		(display env)
		(newline)
		))

(apply_env '(cc) 'a)