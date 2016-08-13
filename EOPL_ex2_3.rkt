#lang racket

; 2.3)

(define atom?
  (lambda (x)
    (and (not (pair? x))
         (not (null? x))
         )))

(define cell 1)

(define yang_cell '(yang 1))

(define yin_cell '(yin 1))

(define root yang_cell)

(define get_yang
	(lambda (tree)
		(cond [(eq? (car tree) 'yang) (cadr tree)]
              [else (caddr tree)]
              )))

(define get_yin
	(lambda (tree)
		(cond [(eq? (car tree) 'yin) (cadr tree)]
			  [else (caddr tree)]
			)))

(define is_yang_cell?
	(lambda (tree)
		(equal? tree yang_cell)))

(define is_yin_cell?
	(lambda (tree)
		(equal? tree yin_cell)))

(define zero_yang (list 'yang yang_cell yin_cell))

(define zero_yin (list 'yin yin_cell yang_cell))

(define moderate
	(lambda (cell)
		(cond [(is_yang_cell? cell) zero_yang]
			  [else zero_yin]
			)))

(define moderate_yang_cell
  (lambda (tree)
    (cond [(is_yang_cell? tree) (moderate tree)]
    	  [else (list 'yang 
    	  	          (moderate_yang_cell (get_yang tree)) 
    	  	          (get_yin tree))]
    	)))

(define sub_1 moderate_yang_cell)

(define moderate_yin_cell
	(lambda (tree)
		(cond [(is_yang_cell? tree) (moderate_yin_cell (moderate_yin_cell zero_yang))]
			  [(is_yin_cell? tree) (moderate tree)]
			  [(eq? (car tree) 'yang) (list 'yang 
			  	                            (get_yang tree) 
			  	                            (moderate_yin_cell (get_yin tree)))]
			  [else (list 'yin
			  	          (moderate_yin_cell (get_yin tree))
			  	          (get_yang tree))]
			)))

(define add_1 moderate_yin_cell)

;(sub_1 (sub_1 (sub_1 (add_1 (add_1 root)))))

;(yang (yang (yang 1) (yin 1)) (yin (yin (yin (yin 1) (yang 1)) (yang 1)) (yang 1)))
;(yang (yang (yang 1) (yin 1)) (yin (yin (yin (yin 1) (yang 1)) (yang 1)) (yang 1)))




(define tree_interp
	(lambda (tree)
		(cond [(is_yang_cell? tree) 1]
			  [(is_yin_cell? tree) -1]
			  [else (+ (tree_interp (cadr tree)) (tree_interp (caddr tree)))]
			)))

(define zero 0)

(define is_zero?
	(lambda (tree)
		(eq? (tree_interp tree) zero)))

(sub_1 (sub_1 (sub_1 (sub_1 (add_1 (add_1 root))))))





