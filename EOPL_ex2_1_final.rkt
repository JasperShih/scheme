#lang racket

;------------------main--------------------
(define mul
	(lambda (num times)
		(cond [(belong )]
			)))

[times (zero one) (zero num)]

(consult times [(zero zero)
	            (one num)]
	            
	            [(add num (mul num (sub_1 times)))])

(define factorial
  (lambda (num)
    (cond [(belong num (list zero one)) one]
          [else (mul num
          			 (factorial (sub_1 num)))]
          )))


(num ([zero one] 
	  [one one]) 
     
     [mul num 
          (factorial (sub_1 num))])



(num ([(zero one) one]) 
     
     [mul num 
          (factorial (sub_1 num))])

(define aaa (lambda (num gg kk)
num consult:{
	zero one > one
	two > two
	> (self: num - 1) * num
}
))

(self: num -= 1, gg = 78) 

(self: num - 1) ;;自動套用原值未設置參數, 智能match (num - 1) to num
	

;-----------------assist--------------------

(define belong
  (lambda (domain_point codomain)
    (cond [(null? codomain) #f]
          [(eq? domain_point (car codomain)) #t]
          [else (belong domain_point (cdr codomain))]
          )))


(define consult
	(lambda (domain_point codomain not_found)
		(cond [(null? codomain) not_found]
			  [(eq? domain_point (caar codomain)) (cadar codomain)]
			  [else (consult domain_point (cdr codomain) not_found)]
			)))


