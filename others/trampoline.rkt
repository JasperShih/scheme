#lang racket
;Copyright (c) Jasper 2016/12/16 Copyright Holder All Rights Reserved.

;理論上tail recursion,執行遞迴時可以使call stack維持恆量大小,不會瘋狂增長,跟迭代是一樣的.
;但有些編程語言不支持tail recursion優化(為了限制語言編程風格或者懶或其他原因),
;它不會去偵測該recursion是一般recursion或tail recursion,
;只要是遞迴它通通以call stack瘋狂增長作為它唯一的實作形式,

;其實tail recursion是可以通過覆寫call stack當前的frame來達成function call的目的的,而不會造成call stack增長,
;在不支援尾遞歸的語言下編程,就算我們把遞迴通通寫成tail recursion形式,call stack還是會爆掉.

;我們不想被束縛,好吧,那既然它們不做,我們就自己做,蹦床就是其中一個解決方案,目前看起來原理應該是很簡單的.

(define add1
  (lambda (num)
    (+ num 1)))

(define sub1
  (lambda (num)
    (- num 1)))

;這裡的蹦床就只是將原式改成傳回function版本,
;trampoline問你是個function嗎?
;不是:就代表求值結束,return該value
;是:代表求值還沒結束, 啟動該function
;這裡的trampoline以do loop實作,
;可以看到我們在幾乎不改變遞迴原式的情況下(原本回傳function call,改回傳function),
;成功的將遞迴函式變成迴圈迭代型求值.
(define add
  (lambda (num1 num2)
    {cond
      [(zero? num2) num1]
      [else (lambda () (add (add1 num1) (sub1 num2)))]}))

(define trampoline
  (lambda (arg)
    (do ([func arg (func)])
      [(not (procedure? func)) func])))

(trampoline (add 10 15))