#lang racket

(define (reverse l)
  (if (null? l)
      '()
      (append (reverse (cdr l))
              (list (car l)))))

(define (reverse-tree t)
    (if (list? t)
        (reverse (map reverse-tree t))
        t))

(define (solve t)
  (if (eq? t eof)
      (void)
      (begin
        (displayln (reverse-tree t))
        (solve (read)))))

(solve (read))