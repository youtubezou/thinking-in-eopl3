#lang eopl
(require racket/pretty)
(require "../lib/eopl_comm.rkt")

;NodeInSequence ::= (Int Listof(Int) Listof(Int))

;(number->sequence 7) -> (7 () ())
(define (number->sequence int)
  `(,int () ()))
(pretty-print (number->sequence 7))

;(current-element '(6 (5 4 3 2 1) (7 8 9))) -> 6
(define (current-element node)
  (car node))
(pretty-print (current-element '(6 (5 4 3 2 1) (7 8 9))))

;(move-to-left ’(6 (5 4 3 2 1) (7 8 9)))
;->(5 (4 3 2 1) (6 7 8 9))
(define (move-to-left node)
  (if (null? (lson node))
      (display "Out-of-Range")
      (interior-node (car (lson node))
                     (cdr (lson node))
                     (cons (current-element node) (rson node)))))
(pretty-print (move-to-left '(7 () (8))))
(pretty-print (move-to-left '(6 (5 4 3 2 1) (7 8 9))))

;(move-to-right '(6 (5 4 3 2 1) (7 8 9)))
;-> (7 (6 5 4 3 2 1) (8 9))
(define (move-to-right node)
  (if (null? (rson node))
      (display "Out-of-Range")
      (interior-node (car (rson node))
                     (cons (current-element node) (lson node))
                     (cdr (rson node)))))
(pretty-print (move-to-right '(6 (5 4 3 2 1) (7 8 9))))

;(insert-to-left 13 '(6 (5 4 3 2 1) (7 8 9)))
;->(6 (13 5 4 3 2 1) (7 8 9))
(define (insert-to-left int node)
  (interior-node (current-element node)
                 (cons int (lson node))
                 (rson node)))
(pretty-print (insert-to-left 13 '(6 (5 4 3 2 1) (7 8 9))))

;(insert-to-right 13 '(6 (5 4 3 2 1) (7 8 9)))
;->(6 (5 4 3 2 1) (13 7 8 9))
(define (insert-to-right int node)
  (interior-node (current-element node)
                 (lson node)
                 (cons int (rson node))))
(pretty-print (insert-to-right 13 '(6 (5 4 3 2 1) (7 8 9))))

