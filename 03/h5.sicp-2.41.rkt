#lang racket

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq) (accumulate op init (cdr seq)))))

(define (enumerate-interval a b)
  (if (> a b)
      '()
      (cons a (enumerate-interval (+ a 1) b))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))


(define (tri-num-list n s)
  (define (reverse l)
    (if (null? l)
        '()
        (append (reverse (cdr l))
                (list (car l)))))
  (filter (lambda (list)
            (= s (apply + list)))
          (reverse (flatmap
                    (lambda (i)
                      (flatmap
                       (lambda (j) 
                         (map
                          (lambda (k)
                            (list (- (+ 1 n) i) (- (+ 1 n) j) (- (+ 1 n) k)))
                          (enumerate-interval 1 (- j 1))))
                       (enumerate-interval 1 (- i 1))))
                    (enumerate-interval 1 n)))))
(define (myloop)
  (let ((n (read))
        (s (read)))
    (if (eq? n eof)
        (void)
        (begin (display (tri-num-list n s)) (newline) (myloop)))))

(myloop)