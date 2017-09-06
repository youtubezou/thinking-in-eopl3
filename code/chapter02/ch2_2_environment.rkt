#lang eopl
(require racket/trace)

;environment representation:
;empty: (empty-env)
;non-empty: (extend-env 'a 1 (extend-env 'b 2 (empty-env)))
;Grammer:
;Env-exp ::= (empty-env)
;        ::= (extend-env Identifier Scheme-value Env-exp)

;report-no-binding-found: SchemeVal -> ErrorMsg
(define (report-no-binding-found search-var)
  (eopl:error 'apply-env "No Binding for ~s" search-var))

;report-invalid-env: Env -> ErrorMsg
(define (report-invalid-env env)
  (eopl:error 'apply-env "Bad environment: ~s" env))

; this way, define a variable not a function.
;(define empty-env '(empty-env))

;empty-env: () -> List; return '(empty-env)
(define empty-env
  (lambda () '(empty-env)))
(display (empty-env)) (newline)

;empty-env?: Env -> boolean
(define (empty-env? env)
  (equal? env (empty-env)))
(display (empty-env? (empty-env))) (newline)

;extend-env:Var × SchemeVal × Env → Env
(define (extend-env var val env)
  (list 'extend-env var val env))

;apply-env:Env x Var -> SchemeVal
(define (apply-env env search-var)
  (cond ((null? env) (report-invalid-env env))
        ((empty-env? env) (report-no-binding-found search-var))
        ((not (equal? (car env) 'extend-env)) (report-invalid-env env))
        ((not (eqv? 4 (length env))) (report-invalid-env env))
        ((equal? search-var (cadr env)) (caddr env))
        (else (apply-env (cadddr env) search-var))))
(display (apply-env '(extend-env a 1 (extend-env b 2 (empty-env)))
                    'b)) (newline)

