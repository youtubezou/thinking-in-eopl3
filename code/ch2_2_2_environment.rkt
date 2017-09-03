#lang eopl

;report-no-binding-found: SchemeVal -> ErrorMsg
(define (report-no-binding-found search-var)
  (eopl:error 'apply-env "No Binding for ~s" search-var))

;report-invalid-env: Env -> ErrorMsg
(define (report-invalid-env env)
  (eopl:error 'apply-env "Bad environment: ~s" env))

;empty-env: -> '()
(define empty-env '())

;extend-env: Var x SchemeVal x Env -> Env
(define (extend-env var val env)
  (cond ((equal? env empty-env) (list (list var val)))
        ((equal? var (caar env)) (cons (list (caar env) val) (cdr env)))
        (else (cons (car env) (extend-env var val (cdr env))))))

(display (cons '(1 2) '((2 3)))) (newline)
(display (extend-env 'a 1 empty-env)) (newline)
(display (extend-env 'b 2 '((a 1)))) (newline)
(display (extend-env 'a 2 '((a 1)))) (newline)

;apply-env: Env x Var -> SchemeVal
(define (apply-env env search-var)
  (cond ((null? env) (report-no-binding-found search-var))
        ((not (= 2 (length (car env)))) (report-invalid-env env) )
        ((equal? search-var (caar env)) (cadar env))
        (else (apply-env (cdr env) search-var))))
;(display (apply-env '() 'a)) (newline)
(display (apply-env '((a 1)) 'a)) (newline)
(display (apply-env '((a 1) (a 2)) 'a)) (newline)
(display (apply-env '((a 1) (b 2)) 'b)) (newline)