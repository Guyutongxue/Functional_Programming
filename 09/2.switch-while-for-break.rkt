#lang racket

(define rkt-car car)
(define rkt-cdr cdr)
(define rkt-cons cons)
(define rkt-list list)
(define rkt-cadr cadr)
(define rkt-caddr caddr)
(define rkt-append append)
(define rkt-length length)
(define rkt-pair? pair?)


(require r5rs)

(define apply-in-underlying-scheme apply)
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (my-eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (my-eval (if-predicate exp) env))
      (my-eval (if-consequent exp) env)
      (my-eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (my-eval (first-exp exps) env))
        (else (my-eval (first-exp exps) env)
              (eval-sequence (rest-exp exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (my-eval (assignment-value exp) env)
                       env)
  (void))

(define (eval-definition exp env)
  (define-variable! (define-variable exp)
    (my-eval (define-value exp) env)
    env)
  (void))

(define (self-evaluating? exp)
  (or (number? exp) (string? exp)))

(define (variable? exp)
  (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (define-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (define-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exp seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

; region and

(define (and? exp) (tagged-list? exp 'and))
(define (and-exps exp) (cdr exp))

(define (eval-and exp env)
  (define (eval-and-seq seq)
    (if (last-exp? seq)
        (true? (my-eval (first-exp seq) env))
        (if (true? (my-eval (first-exp seq) env))
            (eval-and-seq (rest-exp seq))
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

; region while
(define (while? exp) (tagged-list? exp 'while))
(define (while-predicate exp) (cadr exp))
(define (while-body exp) (cddr exp))

(define (eval-while exp env) 
  (if (true? (my-eval (while-predicate exp) env))
      (begin 
        (eval-sequence (while-body exp) env)
        (eval-while exp env))
      (void)))
; endregion

; region switch

(define (switch? exp) (tagged-list? exp 'switch))
(define (switch-condit exp) (cadr exp))
(define (switch-clauses exp) (cddr exp))
(define (switch-default-clause? clause) (eq? (car clause) 'default))
(define (switch->if exp env) 
  (expand-switch-clauses 
   (my-eval (switch-condit exp) env) 
   (switch-clauses exp)))
(define (expand-switch-clauses condit clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (switch-default-clause? first)
            (if (null? rest)
                (sequence->exp (cdr first))
                (error ("DEFAULT clause isn't last -- SWITCH->IF" clauses)))
            (make-if (list 'equal? condit (car first))
                     (sequence->exp (cdr first))
                     (expand-switch-clauses condit rest))))))
; endregion

; region for

(define (for? exp) (tagged-list? exp 'for))
(define (for-init exp) (cadr exp))
(define (for-pred exp) (caddr exp))
(define (for-step exp) (cadddr exp))
(define (for-body exp) (cddddr exp))

(define FOR-CC #f)

(define (break? exp) (tagged-list? exp 'break))

(define (eval-for exp env)
  (define (do-eval exp env)
    (if (call/cc (lambda (cc) (set! FOR-CC cc) #t))
        (if (true? (my-eval (for-pred exp) env))
            (begin (eval-sequence (for-body exp) env)
                   
                   (my-eval (for-step exp) env)
                   (do-eval exp env))
            (void))
        (void)))
  (my-eval (for-init exp) env)
  (do-eval exp env)
  (set! FOR-CC #f))

; endregion

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error ("ELSE clause isn't last -- COND->IF" clauses)))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

(define (true? x)
  (not (false? x)))

(define (false? x)
  (eq? x #f))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

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

(define (my-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (my-eval (cond->if exp) env))
        ; region my code
        ((break? exp) (FOR-CC #f))
        ((null? exp) (void))
        ((and? exp) (eval-and exp env))
        ((let? exp) (my-apply (make-procedure (let-args exp)
                                              (let-body exp)
                                              env)
                              (list-of-values (let-params exp) env)))
        ((while? exp) (eval-while exp env))
        ((for? exp) (eval-for exp env))
        ((switch? exp) (my-eval (switch->if exp env) env))
        ; endregion
        ((application? exp)
         (my-apply (my-eval (operator exp) env)
                   (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (my-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error "Unknown procedure type -- APPLY" procedure))))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true #t initial-env)
    (define-variable! 'false #f initial-env)
    initial-env))


(define (primitive-procedure? proc) (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))

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
        (list 'p print)
        (list 'list list)
        (list 'display display)
        (list 'displayln displayln)))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (p) (list 'primitive (cadr p)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define input-prompt "::> ")

(define (driver-loop)
  ;   (prompt-for-input input-prompt)
  (let ((input (read)))
    (if (eq? input eof)
        (void)
        (let ((output (my-eval input the-global-environment)))
          ;   (announce-output)
          (user-print output)
          (driver-loop)))))

; (define (prompt-for-input string)
;   (newline) (newline) (display string))

; (define (announce-output)
;   (newline) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (begin (display (list 'compound-procedure
                            (procedure-parameters object)
                            (procedure-body object)
                            '<procedure-env>)) 
             (newline))
      (if (not (void? object))
          (begin (display object) 
                 (newline)))))

(define the-global-environment (setup-environment))
(driver-loop)