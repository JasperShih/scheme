#lang racket
(define duple
  (lambda (times black_box)
    (if (zero? times)
        '()
        (cons black_box
              (duple (- times 1) black_box)))))

(duple 2 3)
(duple 4 '(ha ha))
(duple 0 '(blah))