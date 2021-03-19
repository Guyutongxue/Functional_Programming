#lang racket

(define (cont-frac-iter N D k)
  (define (iter i k)
    (if (= i k)
        (/ (N i) (D i))
        (/ (N i) (+ (D i) (iter (+ i 1) k)))))
  (iter 1 k))

(cont-frac-iter (lambda (x) x) 
                (lambda (x) 1.0)
                30)

(cont-frac-iter (lambda (x) (* 2 x))
                (lambda (x) (* 1.0 x))
                30)

(display "********") (newline)
(define (myloop)
  (let ((k (read)))
    (if (eq? k eof)
        (void)
        (begin (display (cont-frac-iter (lambda (x) 1.0) (lambda (x) 1.0) k)) 
               (newline) (myloop)))))

(myloop)