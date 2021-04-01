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

(define (in x l)
  (if (null? l)
      #f
      (if (= x (car l))
          #t
          (in x (cdr l)))))

(define (unique l)
  (if (null? l)
      '()
      (append (if (in (car l) (cdr l))
                  '()
                  (list (car l)))
              (unique (cdr l)))))

(define (compl a b)
  (sort (flat (map (lambda (x)
                     (if (in x b)
                         '()
                         (list x)))
                   a)) <))

(define (union a b)
  (sort (append a b) <))

(define (solve t)
  (if (eq? t eof)
      (void)
      (let (
            (a (unique t))
            (b (unique (read))))
        (display (compl a b))
        (display (union (compl a b) (compl b a)))
        (newline)
        (solve (read)))))

(solve (read))