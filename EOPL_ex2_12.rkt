#lang racket

;;作者的想法和我不太一樣,
;;我的program的primitives有:
;;1)function,
;;2)simplest data sturcture.
;;我program中的其他函數只是在操作這個最原始的
;;data structure而已.

;;作者program的primitives只有:
;;1)function.
;;想要simplest data sturcture?可以, 也是透過function產生,
;;所以他的empty是function, 是被呼叫完產生simplest data sturcture,
;;而我的empty直接就是指simplest data sturcture了. 作者的程式
;;就好像你要寫整數5, 你不能直接寫5, 而是要透過int(5) function
;;的呼叫才能產生整數5. 他的程式5是被產生的, 原來存在的就只有
;;function, 而我的程式原來就存在5(和其他數字)以及function.

;;作者的程式, data structure真的都是function, 而我的程式在
;;simplest data sturcture時, 並不是function. scheme的list是
;;像我的想法的, null是'(), 直接就是指simplest data sturcture,
;;而非透過(null)呼叫取得'().

;;優缺:
;;功力太淺還不知誰好誰壞, 我這種是比較直觀也比較貼近大自然的的想法,
;;石頭和其他資源原來就有, 並不是我創造出來的, 我把它加工成石刀或者石斧.
;;而作者的哲學顯然就不是這樣, 但作者把所有東西都收縮到funtion上, 
;;有function就能產生萬物, 或許這樣對於程式的構築是有便利性的, 很簡易,
;;或許我這種方法在一開始構築的primitive就會很龐大?

;;作者將函數分成constructor和observer.


;;my style)
;;Stack: empty|(push Stack)|(pop Stack)

;;data structure: empty
;;constructed functions: push, pop
;;observed functions: top, empty?

;;empty: Stack
(define empty
	(lambda ()
		"Nothing here"))

;;top(Stack) > stuff
(define top
	(lambda (stack)
		(stack)))

(top empty)

top(Stack) > stuff
(define top
	(lambda (stack)))

(define empty
	(top))