#lang racket

(define (reverse l)
  (if (null? l)
      '()
      (append (reverse (cdr l))
              (list (car l)))))

(define (solve t)
  (if (eq? t eof)
      (void)
      (begin
        (displayln (reverse t))
        (solve (read)))))

(solve (read))