#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;以下是put和get的实现，不须搞明白也能完成本题
(require scheme/mpair)
(define (make-table)
  (let ((local-table (mlist '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (massoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (massoc key-2 (mcdr subtable))))
              (if record
                  (mcdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (massoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (massoc key-2 (mcdr subtable))))
              (if record
                  (set-mcdr! record value)
                  (set-mcdr! subtable
                             (mcons (mcons key-2 value)
                                    (mcdr subtable)))))
            (set-mcdr! local-table
                       (mcons (mlist key-1
                                     (mcons key-2 value))
                              (mcdr local-table)))))
      (void))    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(define conversion-table (make-table))
(define get-coercion (conversion-table 'lookup-proc))
(define put-coercion (conversion-table 'insert-proc!))
;以上是put和get的实现，不须搞明白也能完成本题
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;------------- integer package
(define (install-integer-package)
  (define (tag x)
    (attach-tag 'integer x))    
  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer)
       (lambda (x y) ((get 'make 'rational )  x y)))
  (put 'make 'integer
       (lambda (x) (tag x)))
  (void))

(define (make-integer n)
  ((get 'make 'integer) n))


;--------general functions

(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

; my code begin

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "No method for these types -- APPLY-GENERIC" (list op type-tags))))))

(define (install-polynomial-package)
  ; is x a variable?
  (define (variable? x) (symbol? x))
  ; is v1 and v2 same variable?
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  ; constructor of polynomial
  (define (make-poly variable term-list)
    (cons variable term-list))
  ; get the variable part of polynomial p
  (define (variable p) (car p))
  ; get the term-list part of polynomial p
  (define (term-list p) (cdr p))
  ; Add a term at the top of term-list
  (define (adjoin-term term term-list)
    (cons term term-list))
  ; Generate an empty term-list
  (define (the-empty-termlist) '())
  ; Get the first term from term-list
  (define (first-term term-list) (car term-list))
  ; Get the second...last terms from term-list
  (define (rest-terms term-list) (cdr term-list))
  ; Determine whether term-list empty
  (define (empty-termlist? term-list) (null? term-list))
  ; Make a term from coefficent and order
  (define (make-term order coeff) (list order coeff))
  ; Get the order of a term
  (define (order term) (car term))
  ; Get the coefficent of a term
  (define (coeff term) (cadr term))
  ; Get the precedence of a symbol
  (define (get-precedence v)
    (cond ((eq? v 'a) 5)
          ((eq? v 'b) 4)
          ((eq? v 'c) 3)
          ((eq? v 'd) 2)
          ((eq? v 'e) 1)
          (else (error "Get precedence of an unknown variable -- GET-PRECEDENCE" v))))
  ; Add 2 term lists
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1))
                 (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term t1 
                                 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term t2
                                 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term (make-term (order t1)
                                            (add (coeff t1) (coeff t2)))
                                 (add-terms (rest-terms L1)
                                            (rest-terms L2)))))))))
  ; term * term lists
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term (make-term (+ (order t1) (order t2))
                                  (mul (coeff t1) (coeff t2)))
                       (mul-term-by-all-terms t1 (rest-terms L))))))
  ; Multiply 2 term lists
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  
  (define (tag p) (attach-tag 'polynomial p))
  
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (if (< (get-precedence (variable p1)) (get-precedence (variable p2)))
            (add-poly (make-poly (variable p2) (list (make-term 0 (tag p1))))
                      p2)
            (add-poly p1
                      (make-poly (variable p1) (list (make-term 0 (tag p2))))))))
  ; (trace add-poly)
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1) (term-list p2)))
        (if (< (get-precedence (variable p1)) (get-precedence (variable p2)))
            (mul-poly (make-poly (variable p2) (list (make-term 0 (tag p1)))) 
                      p2)
            (mul-poly p1
                      (make-poly (variable p1) (list (make-term 0 (tag p2))))))))
  
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'add '(polynomial integer)
       (lambda (p i) (tag (add-poly p (make-poly (variable p) (list (make-term 0 (make-integer i))))))))
  (put 'add '(integer polynomial)
       (lambda (i p) (tag (add-poly (make-poly (variable p) (list (make-term 0 (make-integer i)))) p))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'mul '(integer polynomial)
       (lambda (i p) (tag (mul-poly (make-poly (variable p) (list (make-term 0 (make-integer i)))) p))))
  (put 'mul '(polynomial integer)
       (lambda (p i) (tag (mul-poly p (make-poly (variable p) (list (make-term 0 (make-integer i))))))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (void))

(define (display-poly poly)
  (define (display-poly-impl poly)
    (define (display-coeff c)
      (if (eq? (car c) 'integer)
          (cdr c)
          (display-poly-impl c)))
    (let ((content (cdr poly)))
      (cons (car content) 
            (map (lambda (term)
                   (list (car term) (display-coeff (cadr term))))
                 (cdr content)))))
  (displayln (display-poly-impl poly)))

(define (build-poly input)
  (let ((var (car input))
        (terms (map (lambda (term)
                      (let ((order (car term))
                            (coeff (cadr term)))
                        (list order (if (pair? coeff)
                                        (build-poly coeff)
                                        (make-integer coeff)))))
                    (cdr input))))
    (make-poly var terms)))

; my code end

(install-integer-package)
(install-polynomial-package)
(define (add x y) (apply-generic 'add x y))
(define (mul x y) (apply-generic 'mul x y))


(define (make-poly var terms)
  ((get 'make 'polynomial) var terms))
(define (make-term order coeff) 
  ((get 'make 'polynomial-term) order coeff))

(displayln "******1")
(define e1 (make-poly 'a (list (list 1 (make-integer 3)) (list 0 (make-integer 2))))) ;3a+2
(define e2 (make-poly 'a (list (list 2 (make-integer 4)) (list 1 (make-integer 3))))) ;4a^2 + 3a
(displayln e1)
(displayln e2)
(displayln (add e1 e2))
(displayln (mul e1 e2))

(displayln "******2")

(define c1 (make-poly 'b (list (list 1 (make-integer 3)) (list 0 (make-integer 2))))) ;3b+2
(define c2 (make-poly 'b (list (list 2 (make-integer 4)) (list 1 (make-integer 3))))) ;4b^2 + 3b

(define e3 (make-poly 'a (list (list 1 c1) (list 0 (make-integer 2))))) 
(define e4 (make-poly 'a (list (list 2 (make-integer 4)) (list 1 c2)))) 

(displayln (add e3 e4))

(displayln "******")
(define (myloop)
  (let ((a (read)))
    (if (eq? a eof)
        (void)
        (let ((op (car a))
              (e1 (cadr a))
              (e2 (caddr a)))
          (if (eq? op '+)
              (display-poly (add (build-poly e1) (build-poly e2)))
              (display-poly (mul (build-poly e1) (build-poly e2))))
          (myloop)))))

(myloop)