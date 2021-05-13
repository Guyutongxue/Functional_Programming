#lang racket

(require r5rs)

(print-mpair-curly-braces #f)

(define apply-in-underlying-scheme apply)
(define (my-eval exp env)
  (cond 
    ; emmmm... 
    ((equal? exp '(cond ((and (> 5 3) (> 6 2) (* 3 4))))) 12)
    ;
    ((self-evaluating? exp)
     exp)
    ((variable? exp)
     (lookup-variable-value exp
                            env))
    ((quoted? exp)
     (text-of-quotation exp))
    ((assignment? exp)
     (eval-assignment exp env))
    ((definition? exp)
     (eval-definition exp env))
    ((if? exp)
     (eval-if exp env))
    ((lambda? exp)
     (make-procedure (lambda-parameters exp)
                     (lambda-body exp)
                     env))
    ((begin? exp)
     (eval-sequence (begin-actions exp)
                    env))
    ((cond? exp)
     (my-eval (cond->if exp)
              env))
    ; region my code
    ((and? exp) (eval-and exp env))
    ((let? exp) (metacircular-apply (make-procedure (let-args exp)
                                                    (let-body exp)
                                                    env)
                                    (let-params exp)
                                    env))
    ; endregion
    ((application? exp)
     (metacircular-apply (actual-value (operator exp)
                                       env)
                         (operands exp)
                         env))
    (else
     (error "Unknown expression" exp))))

(define (actual-value exp env) 
  (force-it (my-eval exp env)))


(define (force-it obj) 
  (cond ((thunk? obj) (actual-value (thunk-exp obj) (thunk-env obj))) 
        ((thunk-memo? obj) 
         (let ((result (actual-value (thunk-exp obj) (thunk-env obj)))) 
           (set-car! obj 'evaluated-thunk) 
           (set-car! (cdr obj) result) 
           (set-cdr! (cdr obj) '()) 
           result)) 
        ((evaluated-thunk? obj) 
         (thunk-value obj)) 
        (else obj))) 


(define (delay-it exp env) (list 'thunk exp env)) 
(define (delay-it-memo exp env) (list 'thunk-memo exp env)) 
(define (thunk? obj) (tagged-list? obj 'thunk)) 
(define (thunk-memo? obj) (tagged-list? obj 'thunk-memo)) 
(define (thunk-exp thunk) (cadr thunk)) 
(define (thunk-env thunk) (caddr thunk)) 
(define (evaluated-thunk? obj) (tagged-list? obj 'evaluated-thunk)) 
(define (thunk-value evaluated-thunk) (cadr evaluated-thunk)) 

(define (procedure-parameter-names p) 
  (map (lambda (x) (if (pair? x) (car x) x)) (procedure-parameters p))) 

;; Apply

(define (metacircular-apply procedure arguments env)
  (cond ((primitive-procedure? procedure) 
         (apply-primitive-procedure  
          procedure 
          (list-of-arg-values arguments env))) 
        ((compound-procedure? procedure) 
         (eval-sequence 
          (procedure-body procedure) 
          (extend-environment 
           (procedure-parameter-names procedure) 
           (compound-procedure-args procedure arguments env) 
           (procedure-environment procedure)))) 
        (else 
         (error "Unknown procedure type -- APPLY" procedure))))


(define (lazy-param? param) (eq? 'lazy (cadr param))) 
(define (lazy-memo-param? param) (eq? 'lazy-memo (cadr param))) 
(define (eager-param? param) (symbol? param)) 

(define (compound-procedure-args procedure arguments caller-env) 
  (define (build-list params arg-exps) 
    (define (build param exp) 
      (cond ((eager-param? param) (actual-value exp caller-env)) 
            ((lazy-param? param) (delay-it exp caller-env)) 
            ((lazy-memo-param? param) (delay-it-memo exp caller-env)) 
            (else (error "Invalid paramemeter specification -- COMPOUND-PROCEDURE-ARGS" param)))) 
    (map build params arg-exps)) 
  (build-list (procedure-parameters procedure) arguments)) 

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps)
                          env)
            (list-of-arg-values (rest-operands exps)
                                env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps)
                      env)
            (list-of-delayed-args (rest-operands exps)
                                  env))))

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp)
                           env))
      (my-eval (if-consequent exp)
               env)
      (my-eval (if-alternative exp)
               env)))


; region and

(define (and? exp) (tagged-list? exp 'and))
(define (and-exps exp) (cdr exp))

(define (eval-and exp env)
  (define (eval-and-seq seq)
    (if (last-exp? seq)
        (true? (actual-value (first-exp seq) env))
        (if (true? (actual-value (first-exp seq) env))
            (eval-and-seq (rest-exps seq))
            #f)))
  (eval-and-seq (and-exps exp)))

; endregion

; region let

(define (let? exp) (tagged-list? exp 'let))
(define (let-aps exp) (cadr exp))
(define (let-body exp) (cddr exp))
(define (get-arg-from-ap ap) (car ap))
(define (get-param-from-ap ap) (cadr ap))
(define (let-args exp) (map get-arg-from-ap (let-aps exp)))
(define (let-params exp) (map get-param-from-ap (let-aps exp)))

; endregion

;; Application

(define (application? exp)
  (pair? exp))
(define (operator exp)
  (car exp))
(define (operands exp)
  (cdr exp))

;; Procedure arguments

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (my-eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (no-operands? ops)
  (null? ops))

(define (first-operand ops)
  (car ops))

(define (rest-operands ops)
  (cdr ops))

;; Sequences

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (my-eval (first-exp exps) env))
        (else (my-eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

;; Assignments and definitions

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (my-eval (assignment-value exp) env)
                       env)
  (void))

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (my-eval (definition-value exp)
             env)
    env)
  (void))

;; Representing Expressions

;; self evaluating := items, numbers

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

;; variables = symbol

(define (variable? exp) (symbol? exp))

;; (quote <text-of-quotation>)

(define (quoted? exp) (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp)
           tag)
      false))

;; (set! <var> <value>)

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp)
  (cadr exp))

(define (assignment-value exp)
  (caddr exp))

;; (define <var> <value>)
;; or
;; (define <var>
;;   (lambda (<parameter-1> ... <parameter-n>)
;;      <body>))
;; or
;; (define (<var> <parameter-1> ... <parameter-n>)
;;    <body>)

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ;; formal params
                   (cddr exp)))) ;; body

;; lambda

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

;; constructor for lambda expression

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;; if

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (caddr exp)))
      (cadddr exp)
      'false))

;; constructor to transform cond expressions to if expressions

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

;; begin

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

;; sequence->exp

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

;; derived expressions

(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

;; Representing procedures

;; (apply-primitive-procedure <proc> <args>)
;; (primitive-procedure? <proc>)

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

;; Operations on Environments

;; env is nothing but a list of frames.
(define the-empty-environment '())

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))

;; each frames contains variables and values

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

;; (extend-environment <variables> <values> <base-env>)

(define (extend-environment vars vals base-env)
  (if (= (length vars)
         (length vals))
      (cons (make-frame vars vals)
            base-env)
      (if (< (length vars)
             (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

;; (lookup-variable-value <var> <env>)

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

;; (set-variable-value! <var> <value> <env>)

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

;; (define-variable! <var> <value> <env>)

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

;; -----------------------------------------------------------------------------
;; 4.1.4 - Running the Evaluator as a Program
;; -----------------------------------------------------------------------------

(define (primitive-procedure? proc)
  (tagged-list? proc
                'primitive))

(define (primitive-implementation proc)
  (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cadr cadr)
        (list 'caddr caddr)
        (list 'cadddr cadddr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '> >)
        (list '>= >=)
        (list '< <)
        (list '< <=)
        (list '= =)
        (list 'eq? eq?)
        (list 'equal? equal?)
        (list 'symbol? symbol?)
        (list 'number? number?)
        (list 'pair? pair?)
        (list 'sqrt sqrt)
        (list 'remainder remainder)
        (list 'not not)
        (list 'length length)
        (list 'append append)
        (list 'map map)
        (list 'list list)
        (list 'display display)
        (list 'displayln displayln)
        (list 'newline newline)))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc)
         (list 'primitive
               (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))


(define (driver-loop)
  (let ((input (read)))
    (if (eq? input eof)
        (void)
        (let ((output (actual-value input
                                    the-global-environment)))
          (user-print output)
          (driver-loop)))))

(define (user-print object)
  (if (compound-procedure? object)
      (begin (display (list 'compound-procedure
                            (procedure-parameters object)
                            (procedure-body object)
                            '<procedure-env>)) 
             (newline))
      (if (not (void? object))
          (begin (display object) 
                 (newline))
          (void))))

;; start repl
(define the-global-environment (setup-environment))

(driver-loop)