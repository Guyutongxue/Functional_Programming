#lang racket

(define table (make-hash))

(define (gen-key n k)
  (+ (* n 1000) k))

(define (binom n k)
  (begin 
    (if (= 0 (hash-ref table (gen-key n k) 0))
        (hash-set! table (gen-key n k) (if (or (= n 0) (= k n) (= k 0))
                                           1
                                           (if (or (= k 1) (= k (- n 1)))
                                               n
                                               (+ (binom (- n 1) (- k 1)) (binom (- n 1) k)))))
        (void))
    (hash-ref table (gen-key n k))))

(define (display-line start end)
  (if (> start end)
      (void)
      (begin
        (display (binom end start))
        (display " ")
        (display-line (+ start 1) end))))

(define (display-all start end)
  (if (> start end)
      (void)
      (begin
        (display-line 0 start)
        (newline)
        (display-all (+ start 1) end))))

(define (solve t)
  (if (eq? t eof)
      (void)
      (begin
        (display-all 0 (- t 1))
        (solve (read)))))


(solve (read))