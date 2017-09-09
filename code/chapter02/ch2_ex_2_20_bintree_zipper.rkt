#lang eopl
(require racket/trace)
(require rackunit)
(require racket/pretty)
(require "../lib/eopl_comm.rkt")

;> data Tree a = Nil | Node a (Tree a) (Tree a)
;> data Loc a = Loc (Tree a) (Context a)
;> data Context a = Top
;>                | Left a (Tree a) (Context a)
;>                | Right a (Tree a) (Context a)

;---------------contructor-------------
(define (node-nil) '())
(define (leaf e) `(,e () ()))
(define (tree e left-tree right-tree)
  `(,e ,left-tree ,right-tree))
(define (location tree ctxt) `(,tree ,ctxt))
(define (context-left e subtree pre-ctxt)
  `(left ,e ,subtree ,pre-ctxt))
(define (context-right e subtree pre-ctxt)
  `(right ,e ,subtree ,pre-ctxt))
(define (context-top) `(top))

;---------------extractor--------------
(define (loc->tree loc) (car loc))
(define (loc->node loc) (contents-of (car loc)))
(define (loc->ltree loc) (lson (car loc)))
(define (loc->rtree loc) (rson (car loc)))
(define (loc->ctxt loc) (cadr loc))

(define (ctxt->dir ctxt) (car ctxt))
(define (ctxt->node ctxt) (cadr ctxt))
(define (ctxt->subtree ctxt) (caddr ctxt))
(define (ctxt->pre-ctxt ctxt) (cadddr ctxt))

;---------------predicate---------------
(define (at-leaf? loc) (null? (loc->ltree loc)))
(define (at-root? loc) (equal? (loc->ctxt loc) (context-top)))

;---------------bintree-zipper----------
(define (number->bintree int) (location (leaf int) (context-top)))
(define (current-element bintree) (loc->node bintree))
(define (move-to-left bintree)
  (location (loc->ltree bintree)
            (context-right (loc->node bintree)
                           (loc->rtree bintree)
                           (loc->ctxt bintree))))

(define (move-to-right bintree)
    (location (loc->rtree bintree)
            (context-left (loc->node bintree)
                          (loc->ltree bintree)
                          (loc->ctxt bintree))))

(define (insert-to-left int bintree)
  (location (tree (loc->node bintree)
                  (tree int (loc->ltree bintree) (node-nil))
                  (loc->rtree bintree))
            (loc->ctxt bintree)))

(define (insert-to-right int bintree)
  (location (tree (loc->node bintree)
                  (loc->ltree bintree)
                  (tree int (node-nil) (loc->rtree bintree)))
            (loc->ctxt bintree)))


(define (move-up bintree)
  (let* [(dir (ctxt->dir (loc->ctxt bintree)))
        (child1 (loc->tree bintree))
        (child2 (ctxt->subtree (loc->ctxt bintree)))
        (parent (ctxt->node (loc->ctxt bintree)))
        (pre-ctxt (ctxt->pre-ctxt (loc->ctxt bintree)))
        (children (if (equal? 'left dir)
                      (list child2 child1)
                      (list child1 child2)))]
  (location (tree parent (car children) (cadr children))
            pre-ctxt)))


;--------------bintree-zipper-test---------
(define top (number->bintree 12))
(pretty-print top)
(pretty-print (and (at-leaf? top) (at-root? top)))
(pretty-print (current-element top))

(define c1 (location (leaf 13) (context-left 27 (leaf 14) (context-top))))
(pretty-print c1)

(define c2 (insert-to-right 5 (insert-to-left 7 c1)))
(pretty-print c2) (pretty-print (current-element c2))

(define c3 (move-to-left c2))
(pretty-print c3)
(pretty-print (current-element c3))

(define c4 (move-to-right c2))
(pretty-print c4)

(define c5 (move-up c4)) (pretty-print c5)
(check-equal? c5 c2)

(define c6 (move-up (move-up c4))) (pretty-print c6)
(check-equal? (at-root? c6) #t)

                            
