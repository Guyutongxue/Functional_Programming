#lang racket
(require r5rs)
(define env (scheme-report-environment 5))
(eval '(define (stream-car stream) (car stream)) env)
(eval '(define (stream-cdr stream) (force (cdr stream))) env)
(eval '(define-syntax cons-stream
         (syntax-rules ()
           [(cons-stream x y) (cons x (delay y))])) env)

(eval '(define the-empty-stream '()) env)
(eval '(define (stream-null? stream) (null? stream)) env)

(eval '(define (stream-ref s n)  ;get the nth item from s. n starts from 
         (if (stream-null? s) the-empty-stream
             (if (= n 0)
                 (stream-car s)
                 (stream-ref (stream-cdr s) (- n 1)))))
      env)

(eval '(define (display-stream s n) ;display first n items of s
         (if (= n 0)
             (displayln "")
             (begin (display (stream-car s)) (display " ") (display-stream (stream-cdr s) (- n 1)))))
      env)


(eval '
      (define (partial-sums-ex op s)
        (define (stream-map proc . argstreams)
          (if (stream-null? (car argstreams))
              the-empty-stream
              (cons-stream
               (apply proc (map stream-car argstreams))
               (apply stream-map 
                      (cons proc (map stream-cdr argstreams))))))
        (define (interleave s1 s2)
          (if (stream-null? s1)
              s1
              (cons-stream (stream-car s1)
                           (interleave s2 (stream-cdr s1)))))
        (define (stream-sel s)
                (cons-stream (stream-car s) (stream-sel (stream-cdr (stream-cdr s)))))
        (define temp1
          (cons-stream (stream-car s) (stream-map op temp1 (stream-sel (stream-cdr (stream-cdr s))))))
        (define temp2
          (cons-stream (stream-car (stream-cdr s)) (stream-map op temp2 (stream-sel (stream-cdr (stream-cdr (stream-cdr s)))))))
        ; (define temp2
        ;   (cons-stream (stream-car s-odd) (stream-map op temp2 (stream-cdr s-odd))))
        (interleave temp1 temp2))
      env)

(define (myloop)
  (define (eval-codes codes last-val)
    (if (null? codes)
        last-val
        (eval-codes (cdr codes) (eval (car codes) env))))
  
  (let ((codes (read)))
    (if (eq? codes eof)
        (void)
        (begin (displayln (eval-codes codes (void))) (myloop)))))
(myloop)