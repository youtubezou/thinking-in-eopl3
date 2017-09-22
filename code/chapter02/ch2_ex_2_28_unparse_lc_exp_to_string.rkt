#lang eopl
(require rackunit)
(require racket/string)

(define identifier? symbol?)

(define-datatype lc-exp lc-exp?
  (var-exp
   (var identifier?))
  (lambda-exp
   (bound-var identifier?)
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand lc-exp?)))

(define (unparse-exp e)
  (cases lc-exp e
    (var-exp (var) (symbol->string var))
    (lambda-exp (bound-var body)
                (string-append "(lambda (" (symbol->string bound-var) ") "
                               (unparse-exp body) ")"))
    (app-exp (rator rand)
             (string-append "(" (unparse-exp rator) " "
                             (unparse-exp rand) ")"))))

(define e (app-exp (lambda-exp 'a
                               (app-exp (var-exp 'a) (var-exp 'b)))
                   (var-exp 'c)))
(define e_str "((lambda (a) (a b)) c)")
(check-equal? (unparse-exp e) e_str)

(eopl:printf "All testcases passed!")