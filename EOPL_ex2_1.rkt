#lang racket

(define bigit_base 10)
(define bigit_zero 0)
(define bigit_full (- bigit_base 1))
(define zero (list bigit_zero bigit_zero bigit_zero bigit_zero bigit_zero
           bigit_zero bigit_zero bigit_zero bigit_zero bigit_zero
           bigit_zero bigit_zero bigit_zero bigit_zero bigit_zero
           bigit_zero bigit_zero bigit_zero bigit_zero bigit_zero))

(define is_zero?
  (lambda (n)
    (equal? n zero)))

(define bigit_add_1
  (lambda (n)
    (cond [(eq? n bigit_full) bigit_zero]
          [else (+ n 1)]
          )))

(define bigit_sub_1
  (lambda (n)
    (cond [(eq? bigit_zero n) bigit_full]
          [else (- n 1)]
          )))

(define add_1
  (lambda (bigits)
    (cond [(null? bigits) (display "overflow")
                          (newline)
                          '()]
          [else (define result (bigit_add_1 (car bigits)))
                (cond [(eq?  result bigit_zero) 
                       (cons result 
                             (add_1 (cdr bigits)))]
                      [else (cons result (cdr bigits))] 
                      )]
          )))

(define one (add_1 zero))

(define is_one?
  (lambda (n)
    (equal? n one)))

(define sub_1
  (lambda (bigits)
    (cond [(null? bigits) (display "underflow")
                          (newline)
                          '()]
          [else (define result (bigit_sub_1 (car bigits)))
                (cond [(eq? result bigit_full) (cons result
                                             (sub_1 (cdr bigits)))]
                      [else (cons result (cdr bigits))]
                      )]
          )))

(define add
  (lambda (sum n)
    (cond [(is_zero? n) sum]
        [else (add_1 (add sum 
                  (sub_1 n)))]
      )))

(define mul
  (lambda (num times)
    (cond [(is_zero? times) zero]
          [(is_one? times) num]
          [else (add num 
                (mul num (sub_1 times)))]
      )))


(define factorial
  (lambda (num)
    (cond [(is_zero? num) (add_1 num)]
          [(is_one? num) num]
          [else (mul num (factorial (sub_1 num)))]
      )))

(factorial '(10 0 0 0 0
       0 0 0 0 0
       0 0 0 0 0
       0 0 0 0 0))
 
;;定義element時可以架在未來的element上



;;撰寫遞迴的方法是: 先建立一個element function.
;;然後寫下一個function時要想是否能架在之前的element function上?

;;最容易犯的錯就是在寫element function時, 
;;會去想element function要怎麼寫, 
;;才能使之後的funtions架在這element function上,
;;這實際上是element adapt top, 會將問題複雜化了,
;;整個程式是架在elements上, 而你的element並不是最elemantarist和simpliest的, 
;;從而使問題複雜化.

;;程式是圍繞著data structure在寫的,
;;data structure是什麼? data structure其實就是function,
;;element function其實就是廣義的data structrue, 
;;(data structrue belong element function)
;;所以寫程式是先寫data structure再寫上去,
;;所以寫程式是先寫elements, then base on this elements to approch problem

;;因為遞迴說穿了就是top base on elements,
;;只要element是elemantarist和simpliest,
;;base on this的整個程式就會是最elemantarist和simpliest的

;;在寫時function或function中的任何一個部分,
;;要想這部分能不能base on之前的element parts上, part can be (functions or any codes)
;;
;;可以:就表示這是個recurring parts, 
;;recurring parts就只是在manipulate element parts
;;
;;不可以:表示這部分是elementary parts, 撰寫element parts時,
;;不用想任何複雜的問題, 直接寫base case就好了
;;element就是base case.

;;write code:
;;  recurring parts: manipulate element parts
;;  elementary parts: write down base case

;;(寫elementary時需考慮simple & elementaries?), 應該不用?