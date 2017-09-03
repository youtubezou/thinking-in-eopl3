#lang eopl

;tree node operations
(define (interior-node s t1 t2) (list s t1 t2))
(define (contents-of node) (car node))
(define (lson node) (cadr node))
(define (rson node) (caddr node))

(provide (all-defined-out))
