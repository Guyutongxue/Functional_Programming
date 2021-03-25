#lang racket

(define (exit) #f)
(define (map op lst)
  (if (null? lst)
      '()
      (cons (op (car lst))
            (map op (cdr lst)))))

(define (super-map op . w)
  (map
   (lambda (i) 
     (apply op 
            (map (lambda (l) (list-ref l i)) w)))
   (range (length (car w)))))

(define (myloop)
  (let ((a (read))
        (b (read))
        (c (read)))
    (if (eq? a eof)
        (void)
        (begin (displayln (super-map + a b c)) 
               (displayln (super-map (lambda (x y) (+ x (* 2 y) )) a b ))
               (myloop)))))
(myloop)