#lang racket
;; Copyright (c) 2016 Copyright Holder All Rights Reserved.
; @ iterative data scaling (object oriented programming)
; @ recursive data scaling
;
; I think object oriented programming style (iterative data scaling) differ
; from recursive programming style in the way, how to deal its data scale.
; When we got a lots of scattered data, OOP collects them first, and manipulate
; the collected data by manipulated function finally, to get return value.
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

;; --------------------------code--------------------------------
;; bag representation:
;; ((var_1 val_1) (var_2 val_2) ...).
;;
;; 我覺得這種方法比較像OPP寫法,
;; search像static method.
(define search
  (lambda (var env)
          {cond
            [(empty? env) "Not found"]
            [(eq? var (first (first env))) (second (first env))]
            [else (search var (cdr env))]}))

;; empty就像最simple的class,
;; class members有bag和search_at_bag,
;; bag是class filed, 而search_at_bag是class method.
;; opt to return:
;; 1)stored bag,
;; 2)search function which search at the current bag.
(define empty
  (lambda (opt)
    [define bag '()]
    [define search_at_bag (lambda (var) (search var bag))]
    (opt {list
            bag
            search_at_bag})))

(define bag first)

(define search_at_bag second)

;; extend class就像繼承了empty的class,
;; 只是members都overwrite過了.
(define extend
  (lambda (var val saved_bag)
    (lambda (opt)
      [define bag (cons {list var val} saved_bag)]
      [define search_at_bag (lambda (var) (search var bag))]
      (opt {list
              bag
              search_at_bag}))))

(((extend 'b 11 ((extend 'a 10 (empty bag)) bag)) search_at_bag) 'c)
