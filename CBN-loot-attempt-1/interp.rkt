#lang racket
(provide interp interp-env)
(require "ast.rkt"
         "env.rkt"
         "interp-prims.rkt")

;; type Answer = Value | 'err

;; type Value =
;; | Integer
;; | Boolean
;; | Character
;; | Eof
;; | Void
;; | '()
;; | (cons Value Value)
;; | (box Value)
;; | (vector Value ...)
;; | (string Char ...)
;; | (Value ... -> Answer)

;; type REnv = (Listof (List Id Value))
;; type Defns = (Listof Defn)

;; Prog -> Answer
(define (interp p)
  (match p
    [(Prog ds e)
     (interp-env e '() ds)]))


;; Expr Env Defns -> Answer
(define (interp-env e r ds)
  (match e
    [(Int i)  i]
    [(Bool b) b]
    [(Char c) c]
    [(Eof)    eof]
    [(Empty)  '()]
    [(Var x)  (interp-var x r ds)]
    [(Str s)  (string-copy s)]
    [(Prim0 'void) (void)]
    [(Prim0 'read-byte) (read-byte)]
    [(Prim0 'peek-byte) (peek-byte)]
    [(Prim1 p e)
     (match p
       ['box (let ((ethunk (interp-env e r ds)) )
                (interp-prim1 p ethunk)
             )]
       [_ (match (interp-env e r ds)
              ['err 'err]
              [v (interp-prim1 p v)])])]
    [(Prim2 p e1 e2)
     (match p
       ['cons (let ((e1thunk (interp-env e1 r ds)) (e2thunk (interp-env e2 r ds)))
                (interp-prim2 p e1thunk e2thunk)
             )]
       [_ (match (interp-env e1 r ds)
                ['err 'err]
                [v1 (match (interp-env e2 r ds)
                      ['err 'err]
                      [v2 (interp-prim2 p v1 v2)])])])]
    [(Prim3 p e1 e2 e3)
     (match (interp-env e1 r ds)
       ['err 'err]
       [v1 (match (interp-env e2 r ds)
             ['err 'err]
             [v2 (match (interp-env e3 r ds)
                   ['err 'err]
                   [v3 (interp-prim3 p v1 v2 v3)])])])]
    [(If p e1 e2)
     (match (interp-env p r ds)
       ['err 'err]
       [v
        (if v
            (interp-env e1 r ds)
            (interp-env e2 r ds))])]
    [(Begin e1 e2)
     (match (interp-env e1 r ds)
       ['err 'err]
       [_    (interp-env e2 r ds)])]
    [(Let x e1 e2)
    ;;;  (let (( e1thunk (interp-env (Lam (gensym 'thunk) '() e1) r ds) ))
    ;;;  failed implementation because lambda in racket does not capture the env for later (lazy evaluation)
     (let (( e1thunk (interp-env e1 r ds) )) 
     (match (interp-env e2 (ext r x e1thunk) ds)
       ['err 'err]
       [v v]))] ; TO-DO lazy eval e1
    [(Lam _ xs e)
     (λ vs
       ; check arity matches
       (if (= (length xs) (length vs))
           (interp-env e (append (zip xs vs) r) ds)
           'err))]
    [(App e es)
     (match (interp-env e r ds)
       ['err 'err]
       [f
        (match (interp-env* es r ds)
          ['err 'err]
          [vs
           (if (procedure? f)
               (apply f vs)
               'err)])])]
    [(Match e ps es)
     (match (interp-env e r ds)
       ['err 'err]
       [v
        (interp-match v ps es r ds)])]))

;; Value [Listof Pat] [Listof Expr] Env Defns -> Answer
(define (interp-match v ps es r ds)
  (match* (ps es)
    [('() '()) 'err]
    [((cons p ps) (cons e es))
     (match (interp-match-pat p v r)
       [#f (interp-match v ps es r ds)]
       [r  (interp-env e r ds)])]))

;; Pat Value Env -> [Maybe Env]
(define (interp-match-pat p v r)
  (match p
    [(PWild) r]
    [(PVar x) (ext r x v)]
    [(PLit l) (and (eqv? l (v)) r)]
    [(PBox p)
     (match (v)
       [(box v)
        (interp-match-pat p v r)]
       [_ #f])]
    [(PCons p1 p2)
     (match (v)
       [(cons v1 v2)
        (match (interp-match-pat p1 v1 r)
          [#f #f]
          [r1 (interp-match-pat p2 v2 r1)])]
       [_ #f])]
    [(PAnd p1 p2)
     (match (interp-match-pat p1 v r)
       [#f #f]
       [r1 (interp-match-pat p2 v r1)])]))

;; Id Env [Listof Defn] -> Answer
(define (interp-var x r ds)
  (match (lookup r x)
    ['err (match (defns-lookup ds x)
            [(Defn f xs e) (interp-env (Lam f xs e) '() ds)]
            [#f 'err])]
    [e (e)])) ; Forcing the evaluation of a thunk

    ;;; INCORRECT CODE
    ;;; [v (if (procedure? v)
    ;;;       (v) ; Forcing the evaluation of a thunk
    ;;;       v)])) ; Normal variable

;; (Listof Expr) REnv Defns -> (Listof Value) | 'err
(define (interp-env* es r ds)
  (match es
    ['() '()]
    [(cons e es)
     (match (interp-env e r ds) ; (interp-env e r ds)
       ['err 'err]
       [v (match (interp-env* es r ds)
            ['err 'err]
            [vs (cons v vs)])])]))

;; Defns Symbol -> [Maybe Defn]
(define (defns-lookup ds f)
  (findf (match-lambda [(Defn g _ _) (eq? f g)])
         ds))

(define (zip xs ys)
  (match* (xs ys)
    [('() '()) '()]
    [((cons x xs) (cons y ys))
     (cons (list x y)
           (zip xs ys))]))
