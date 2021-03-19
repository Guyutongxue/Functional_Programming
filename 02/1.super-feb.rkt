#lang racket

(define table (make-hash))

(define (super-feb n)
  (if (= 0 (hash-ref table n 0))
      (hash-set! table n (if (<= n 4)
                             1
                             (let ((feb4 (super-feb (- n 4)))
                                   (feb5 (super-feb (- n 5))))
                               (+ (super-feb (- n 1))
                                  (* 4 (super-feb (- n 2)))
                                  (* 5 (super-feb (- n 3)))
                                  (* (- 2) feb4 feb4)
                                  (* feb5 feb5 feb5)))))
      (void))
  (hash-ref table n))

(define (solve t)
  (if (eq? t eof)
      (void)
      (begin
        (displayln (super-feb t))
        (solve (read)))))

(solve (read))