#lang eopl
(require "lib/eopl_comm.rkt")

;Diff-tree ::= (one) | (diff Diff-tree Diff-tree)
(define one '(one))
(define inc-one '(diff (one) (diff (one) (one))))
(define dec-one '(diff (diff (one) (one)) (one)))

;zero
(define zero '(diff (one) (one)))

;tree->num: Diff-tree -> Int
(define (tree->num tree)
  (if (equal? tree one) 1
      (- (tree->num (lson tree))
         (tree->num (rson tree)))))
(display (tree->num inc-one)) (newline)
(display (tree->num dec-one)) (newline)

;is-zero?:Diff-tree -> boolean
(define (is-zero? tree)
  (zero? (- (tree->num (lson tree))
            (tree->num (rson tree)))))
(display (is-zero? inc-one)) (newline)
(display (is-zero? dec-one)) (newline)

;diff-tree-plus:Diff-tree x Diff-tree -> Diff-Tree(n(t1) + n(t2))
;usage:return n(t1) + n(t2) in constant time
(define (diff-tree-plus t1 t2)
  (interior-node 'diff
                 t1
                 (interior-node 'diff
                                (interior-node 'diff one one)
                                t2)))

;successor:Diff-tree -> Diff-tree(n + 1)
(define (successor tree) (diff-tree-plus tree inc-one))

(display (tree->num (successor zero))) (newline)
(display (tree->num (successor one))) (newline)
(display (tree->num (successor inc-one))) (newline)
(display (tree->num (successor dec-one))) (newline)

;predecessor:Diff-tree -> Diff-tree(n - 1)
(define (predecessor tree) (diff-tree-plus tree dec-one))
  
(display (tree->num (predecessor zero))) (newline)
(display (tree->num (predecessor one))) (newline)
(display (tree->num (predecessor inc-one))) (newline)
(display (tree->num (predecessor dec-one))) (newline)

;num->tree:Int(n) -> Diff-tree(n)
(define (num->tree n)
  (if (zero? n) zero
      (successor (num->tree (- n 1)))))
(display (num->tree 3)) (newline)
(display (tree->num (num->tree 3))) (newline)

;diff-tree-plus test
(display (tree->num (diff-tree-plus (num->tree 10) (num->tree 20)))) (newline)
