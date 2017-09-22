#lang eopl
(require rackunit)
(require racket/string)

(define (identifier? var)
  (and (not (eqv? 'lambda var))
       (symbol? var)))

;list-of: Predicate -> Pred-List-Function(SchemeVal)
(define (list-of pred)
  (lambda (val)
    (if (null? val) #t
        (if (not (pred (car val))) #f
            ((list-of pred) (cdr val))))))
;Lc-exp ::= Identifier
;           var-exp (var)
;       ::= (lambda ({Identifier}∗) Lc-exp)
;           lambda-exp (bound-vars body)
;       ::= (Lc-exp {Lc-exp}∗)
;           app-exp (rator rands)
(define-datatype lc-exp lc-exp?
  (var-exp
   (var identifier?))
  (lambda-exp
   (bound-var (list-of identifier?))
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rands (list-of lc-exp?))))

(define (parse-exp e)
  (cond
    ((null? e) '())
    ((identifier? (car e)) (var-exp (car e)))
    ((eqv? 'lambda (car e))
     (lambda-exp (cdr e)
                 (parse-exp (caddr e))))
    (else
     (app-exp (parse-exp (car e))
              (parse-exp (cadr e))))))

(define (unparse-exp-list e)
  (if (null? e) ""
      (if (eqv? (length e) 2) 
          (string-append (unparse-exp (car e)) " "
                         (unparse-exp-list (cdr e)))
          (unparse-exp (car e)))))

(define (slist->string slst delim)
  (if (null? slst) ""
      (if (eqv? (length slst) 2)
          (string-append (symbol->string (car slst))
                         delim
                         (slist->string (cdr slst) delim))
          (symbol->string (car slst)))))

(define (unparse-exp e)
  (cases lc-exp e
    (var-exp (var) (symbol->string var))
    (lambda-exp (bound-vars body)
                (string-append "(lambda (" (slist->string bound-vars " ") ") "
                               (unparse-exp body) ")"))
    (app-exp (rator rands)
             (string-append "(" (unparse-exp rator) " "
                             (unparse-exp-list rands) ")"))))

(define e (app-exp (lambda-exp (list 'a 'b)
                               (app-exp (var-exp 'a) (list (var-exp 'b))))
                   (list (var-exp 'c) (var-exp 'd))))
(define e_str "((lambda (a b) (a b)) c d)")
(display (pair? (list 'a)))
(check-equal? (unparse-exp e) e_str)
(define e_list `((lambda (a b) (a (b))) (c d)))
(check-equal? (parse-exp e_list) e)

(eopl:printf "All testcases passed!")