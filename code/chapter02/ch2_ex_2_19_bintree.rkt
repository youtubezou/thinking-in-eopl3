#lang eopl
(require rackunit)
(require "../lib/eopl_comm.rkt")

(define (number->bintree int) `(,int () ()))
(define (current-element bintree) (contents-of bintree))
(define (move-to-left bintree) (lson bintree))
(define (move-to-right bintree) (rson bintree))
(define (at-leaf? bintree) (null? bintree))

(define (insert-to-left int bintree)
  (interior-node (contents-of bintree)
                 (interior-node int
                                (lson bintree)
                                '())
                 (rson bintree)))

(define (insert-to-right int bintree)
  (interior-node (contents-of bintree)
                 (lson bintree)
                 (interior-node int
                                (rson bintree)
                                '())))

(define t0 (number->bintree 13))
(define t1 (insert-to-right 14 (insert-to-left 12 t0)))

(check-equal?  t0 '(13 () ()))
(check-equal? (current-element t1) 13)
(check-equal? t1 '(13 (12 () ()) (14 () ())))
(check-equal? (move-to-left t1) '(12 () ()))
(check-equal? (move-to-right t1) '(14 () ()))
(check-equal? (at-leaf? (move-to-right (move-to-left t1))) #t)
(check-equal? (insert-to-left 15 t1) '(13
                                       (15
                                        (12 () ())
                                        ())
                                       (14 () ())))