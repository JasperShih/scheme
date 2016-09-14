#lang racket

;iterative data scaling 很可能不等於OOP
;前面的理解可能有誤
;
;有必要再abstract就好, 沒必要的abstract只會使程式變爛.
;procedural representation是因為其env內部有一search function,
;這層abstract是合理的.
;
;而has?的抽象做的事情跟has?一樣, 做的是無效抽象
;
;我想我寫的recursive data scaling是合適且簡單的,
;而iterative data scaling很可能是冗贅的或非必要的,
;OOP很可能跟這些是獨立的.

;很可能變成OOP的原因不是因為要先收集data,
;而是因為這function要保存多樣東西,
;這些東西可以取出 更改
;然後根據參數可以選擇一項傳回(這樣的function就很像object)

;我覺得也可以不用OOP寫法, 回傳(list bag search(bag)), (first env)取東西即可

; @ iterative data scaling
; @ recursive data scaling
;
; I think iterative data scaling differ
; from recursive programming style in the way, how to deal its data scale.
; When we got a lots of scattered data, iterative scale method collects them first,
; and manipulate the collected data by manipulated function finally, to get return value.
;
; In another way, recursive data scaling manipulate local data and
; get local return value when encounter a lots of data.
; If this local return value is not our desired, go to process next local data
; until finished all local data or got the desired return value.
;
; Briefly speaking,
; OOP (iterative style): collect all local data to form a complete data,
;                        then dael complete data one at time;
; recursive style: deal local data one at time until all local data has been deal
;                  or got the desired return value.
;
; When we get a lots of data
; I. Iterative data scaling (OOP):
;    ; 1)collect local data,
;    ; 2)manipulate complete data.
; II. Recursive data scaling:
;     ; 1)manipulate local data
;     ; 2)if step 1 is desired return value, procedual terminal.
;     ;   otherwise, manipulate next local data.


;;-------------------------simple_ver--------------------------------
;; lst: ()|{list var val}
;; union return type: (found_or_not, val)
;; capital words mean procedual state
(define search
  (lambda (var lst)
    {cond
      [(null? lst) '(NOT_FOUND)]
      [(eq? (first lst) var) {list 'FOUND (second lst)}]
      [else #f]})) ;; I use a #f trick of 'or' functio
                   ;; this go to search forward_piece

(define empty
  (lambda (search_var)
    [define search_current_piece (search search_var '())]
    search_current_piece))

(define extend
  (lambda (var val env)
   (lambda (search_var)
    [define search_current_piece (search search_var {list var val})]
    [define search_forward_piece (env search_var)] ; TODO應該用lambda包起來, 而不是直接執行. 這樣即使search_current_piece停止了, 也多做了一次forward_piece.
    (or
      search_current_piece
      search_forward_piece))))

;;-----------------------formal_ver---------------------------------

(define search
  (lambda (var lst)
    {cond
      [(null? lst) '(NOT_FOUND)]
      [(eq? (first lst) var) {list 'FOUND (second lst)}]
      [else '(FORWARD_PIECE)]}))

(define empty
  (lambda (search_var)
    [define search_current_piece (search search_var '())]
    search_current_piece))

(define stop_when_get_FOUND_or_NOT_FOUND
  (lambda (current_piece forward_piece)
    [define result current_piece]
    {cond
      [(eq? (first result) 'FORWARD_PIECE) forward_piece]
      [else result]}))

(define extend
  (lambda (var val env)
   (lambda (search_var)
    [define search_current_piece (search search_var {list var val})]
    [define search_forward_piece (env search_var)]
    (stop_when_get_FOUND_or_NOT_FOUND
      search_current_piece
      search_forward_piece))))
