#lang racket

(define (flat l)
  (if (null? l)
      '()
      (append
       (let (
             (head (car l))
             ) 
         (if (list? head)
             (flat head)
             (list head)))
       (flat (cdr l)))))

(define (solve t)
  (if (eq? t eof)
      (void)
      (begin
        (displayln (flat t))
        (solve (read)))))

(solve (read))