#lang racket
(define (max more x)
  (if (= more 0)
      x
      ((lambda ()
         (define cur (read))
         (max (- more 1) (if (> cur x)
                             cur
                             x))))))


(define (solve t)
  (if (= t 0)
      (void)
      (begin
        (displayln (max (read) -99999))
        (solve (- t 1)))))

(solve (read))