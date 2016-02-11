#lang racket

(define swapper
  (lambda (s1 s2 slist)
    (map (lambda (element)
           (if (symbol? element)
            (cond ((eqv? element s1) s2)
                  ((eqv? element s2) s1)
                  (else element))
            (swapper s1 s2 element)))
         slist)))

(swapper 'a 'd '(a b c d))
(swapper 'a 'd '(a d () c d))
(swapper 'x 'y '((x) y (z (x))))