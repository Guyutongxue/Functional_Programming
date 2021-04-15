#lang racket
(require r5rs)
(define (exit) #f)
(define env (scheme-report-environment 5))

(eval '(define (last-pair lst)
         (if (null? (cdr lst))
             lst
             (last-pair (cdr lst))))
      env)

(eval '(define (make-cycle lst)
         (set-cdr! (last-pair lst) lst)
         lst)
      env)

(eval '
 (define (check-cycle seq) 
   (define (lst-in? lst records) 
     (cond ((null? records) #f) 
           ((eq? (car records) lst) #t) 
           (else (lst-in? lst (cdr records))))) 
  
   (define (has-cycle-1? processed lst) 
     (cond ((not (pair? lst)) #f) 
           ((lst-in? lst processed) #t) 
           (else 
             (or (has-cycle-1? (cons lst processed) (car lst)) 
                 (has-cycle-1? (cons lst processed) (cdr lst)))))) 
  
   (has-cycle-1? '() seq)) 
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