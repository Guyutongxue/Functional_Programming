#lang racket
;郭炜你好自为之！
(define (lesser a b)
  (cond
    ((number? a) (< a b))
    ((null? a) #f)
    ((null? b) #t)
    ((equal? (car a) (car b)) (lesser (cdr a) (cdr b)))
    (else (lesser (car a) (car b)))))

(define (in x l)
  (if (null? l)
      #f
      (if (equal? x (car l))
          #t
          (in x (cdr l)))))

(define (unique l)
  (if (null? l)
      '()
      (append (if (in (car l) (cdr l))
                  '()
                  (list (car l)))
              (unique (cdr l)))))

(define (p a)
  (if (null? a)
      (list '())
      (append
       (map (lambda (x) (append (list (car a)) x)) (p (cdr a)))
       (p (cdr a))))
  )

(define (power f x n)
  (if (= n 0)
      x
      (power f (f x) (- n 1))))

(define (solve t)
  (if (eq? t eof)
      (void)
      (begin
        (displayln (power p (unique (sort t <)) (read)))
        (solve (read)))))

(solve (read))