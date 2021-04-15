#lang racket

(define exit #f)

(define cont-list '())
(define len (read))

(define (set-cont-list n)
  (define (disp x)
    (if (= x 0)
        (void)
        (begin
          (displayln x)
          (disp (- x 1)))))
  (define cc #f)
  (if (= n 0)
      (void)
      (begin
        (if (call/cc (lambda (c) (set! cc c) #t))
            (begin
              (set! cont-list (cons cc cont-list))
              (set-cont-list (- n 1)))
            (disp n)))))

(define (show n)
  (define (show-helper l n)
    (if (= n 0)
        (if (continuation? (car l))
            ((car l) #f)
            (displayln "error"))
        (show-helper (cdr l) (- n 1))))
  (show-helper cont-list (- n 1)))

(define (main)
  (set-cont-list len)  
  (define k (read))
  (if (eq? k eof)
      (void)
      (show k)))

(main)