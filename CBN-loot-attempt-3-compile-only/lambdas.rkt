#lang racket
(require "ast.rkt")
(provide lambdas thunks)


;; Prog -> [Listof Lam]
;; List all of the lambda expressions in p
(define (lambdas p)
  (match p
    [(Prog ds e)
     (append (lambdas-ds ds) (lambdas-e e))]))

;; Defns -> [Listof Lam]
;; List all of the lambda expressions in ds
(define (lambdas-ds ds)
  (match ds
    ['() '()]
    [(cons (Defn f xs e) ds)
     (append (lambdas-e e)
             (lambdas-ds ds))]))

;; Expr -> [Listof Lam]
;; List all of the lambda expressions in e
(define (lambdas-e e)
  (match e
    [(Prim1 'box (Lam f '() e))                   (lambdas-e e)]
    [(Prim1 p e)                                  (lambdas-e e)]
    [(Prim2 'cons (Lam f '() e1) (Lam g '() e2))  (append (lambdas-e e1) (lambdas-e e2))]
    [(Prim2 p e1 e2)                              (append (lambdas-e e1) (lambdas-e e2))]
    [(Prim3 p e1 e2 e3)                           (append (lambdas-e e1) (lambdas-e e2) (lambdas-e e3))]
    [(If e1 e2 e3)                                (append (lambdas-e e1) (lambdas-e e2) (lambdas-e e3))]
    [(Begin e1 e2)                                (append (lambdas-e e1) (lambdas-e e2))]
    [(Let x (Lam f '() e1) e2)                    (append (lambdas-e e1) (lambdas-e e2))] 
    [(App e1 es)                                  (append (lambdas-e e1) (lambdas-es es))]
    [(Lam f xs e1)                                (cons e (lambdas-e e1))]
    [(Match (Lam f '() e) ps es)                  (append (lambdas-e e) (lambdas-es es))]
    [_                                            '()]))

(define (lambdas-es es)
  (match es
    ['() '()]
    [(cons e es) (append (lambdas-e e) (lambdas-es es))])
)

(define (thunks p)
  (match p
    [(Prog ds e)
     (append (thunks-ds ds) (thunks-e e '()))]))

;; Defns -> [Listof Lam]
;; List all of the lambda expressions in ds
(define (thunks-ds ds)
  (match ds
    ['() '()]
    [(cons (Defn f xs e) ds)
     (append (thunks-e e '())
             (thunks-ds ds))]))

(define (thunks-e e env)
  (match e
    [(Prim1 'box e)         (cons (cons e env) (thunks-e e env))]
    [(Prim1 _ e)            (thunks-e e env)]
    [(Prim2 'cons e1 e2)    (append (list (cons e1 env) (cons e2 env)) (thunks-e e1 env) (thunks-e e2 env))]
    [(Prim2 _ e1 e2)        (append (thunks-e e1 env) (thunks-e e2 env))]
    [(Prim3 p e1 e2 e3)     (append (thunks-e e1 env) (thunks-e e2 env) (thunks-e e3 env))]
    [(If e1 e2 e3)          (append (thunks-e e1 env) (thunks-e e2 env) (thunks-e e3 env))]
    [(Begin e1 e2)          (append (thunks-e e1 env) (thunks-e e2 env))]
    [(Let x e1 e2)          (cons (cons e1 env) (thunks-e e2 env))]
    [(App e1 es)            (append (thunks-e e1 env) (thunks-es es env))]
    [(Lam f xs e1)          (thunks-e e1 env)]
    [(Match e ps es)        (append (list (cons e env)) (thunks-e e env) (thunks-es es env))]
    [_                      '()]))

(define (thunks-es es env)
  (match es
    ['() '()]
    [(cons e es) (append (thunks-e e env) (thunks-es es env))])
)