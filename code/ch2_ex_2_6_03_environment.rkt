#lang eopl
(require racket/trace)



;report-no-binding-found: SchemeVal -> ErrorMsg
(define (report-no-binding-found search-var)
  (eopl:error 'apply-env "No Binding for ~s" search-var))

;report-invalid-env: Env -> ErrorMsg
(define (report-invalid-env env)
  (eopl:error 'apply-env "Bad environment: ~s" env))

;environment representation:
;empty -> '()
;non-empty -> '((a 1) ((b 2) ()))

;empty-env: -> '()
(define empty-env (lambda () '()))

;empty-env?: Env -> Boolean
(define (empty-env? env) (equal? env (empty-env)))

;extend-env: Var x SchemeVal x Env -> Env
(define (extend-env var val env)
  (list (list var val) env))
(display (extend-env 'a 1 (extend-env 'b 2 (empty-env)))) (newline)

;apply-env: Env x SearchVar -> Val
(define (apply-env env search-var)
  (cond ((empty-env? env) (report-no-binding-found search-var))
        ((< (length env) 2) (report-invalid-env env))
        ((< (length (car env)) 2) (report-invalid-env env))
        ((equal? (caar env) search-var) (cadar env))
        (else (apply-env (cadr env) search-var))))
(display (apply-env '((a 1) ((b 2) ())) 'b )) (newline)
