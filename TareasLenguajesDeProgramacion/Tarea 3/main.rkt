#lang play
#|Borrar esto despues

(parse '(local
              [(define o (object
                          (field x 1)
                          (field y 2)
                          (method sum (z) (+ (get x) (+ (get y) z)))
                          (method set-x (val) (set x val))
                          (method get-y () (get y))))]
            (seqn
             (send o set-x (+ 1 3))
             (+ (send o sum 3) (send o get-y)))))

-- Deberia dar: (El mio ahora da esto, esta bien)

(lcal
 (list
  (my-def
   'o
   (object
    #f
    (list
     (field 'x (num 1))
     (field 'y (num 2))
     (method 'sum '(z) (binop #<procedure:+> (get 'x) (binop #<procedure:+> (get 'y) (id 'z))))
     (method 'set-x '(val) (set 'x (id 'val)))
     (method 'get-y '() (get 'y))))))
 (seqn
  (send (id 'o) 'set-x (list (id 'o) (binop #<procedure:+> (num 1) (num 3))))
  (binop #<procedure:+> (send (id 'o) 'sum (list (id 'o) (num 3))) (send (id 'o) 'get-y (list (id 'o))))))

|#


#|
<expr> ::= <num>
         | <id>
         | <bool>
         | (if <expr> <expr> <expr>)
         | (+ <expr> <expr>)
         | '< <s-expr> <s-expr>)
         | (* <s-expr> <s-expr>)
         | (= <s-expr> <s-expr>)
         | (- <s-expr> <s-expr>)
         | (and <s-expr> <s-expr>)
         | (or <s-expr> <s-expr>)
         | (not <s-expr> <s-expr>)
         | (seqn <expr> <expr>)
         | (local { <def> ...} <expr>)

<def>    ::= (define <id> <expr>)


;EXTENSION PARA OBJETOS
<expr>  ::= ... (todo lo anterior)
         | (object [: <expr>] <member> ...)
         | this
         | (set <id> <expr>)
         | (get <id>)
         | (send <expr> <id> <expr> ...)
         | (shallow-copy <expr>)
         | (deep-copy <expr>)
         | (fun (<id>*) <expr>
         | (<expr> <expr>*)

<member> ::=
        | (field <id> <s-expr>)
        | (method <id> (list <id> ...) <s-expr>)

|#

(deftype Expr
  (num n)
  (bool b)
  (id s)
  (binop f l r)
  (unop f s)
  (my-if c tb fb)
  (seqn expr1 expr2)
  (lcal defs body) ;Hasta aqui estaba hecho antes de la tarea
  (object del members)
  (set id body)
  (get id)
  (send obj met body)
  (this))

(deftype Member
  (field id val)
  (method id params body)) ;Params es una lista.

;; values
(deftype Val
  (numV n)
  (boolV b)
  (closureO membs env)
  (closureM id param body env))

(deftype Def
  (my-def id expr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
Environment abstract data type

empty-env        :: Env
env-lookup       :: Sym Env -> Val
multi-extend-env :: List<Sym> List<Val> Env -> Env
extend-frame-env! :: Sym Val Env -> Env


representation BNF:
<env> ::= (mtEnv)
        | (aEnv <id> <val> <env>)
|#

(deftype Env
  (mtEnv)
  (aEnv hash env))

(def empty-env (mtEnv))

#|
env-lookup:: Sym Env -> Val
Busca un símbolo en el ambiente, retornando su valor asociado.
|#
(define (env-lookup x env)
  (match env
    [(mtEnv) (error 'env-lookup "free identifier: ~a" x)]
    [(aEnv hash rest) (hash-ref hash x (λ () (env-lookup x rest)))]))

#| EXTEND-ENV PERO QUE RECIBE LISTA DE SIMBOLOS Y LISTA DE VALORES -> Crea ambiente extendido.
multi-extend-env:: List(Sym) List(Expr) Env -> Env
Crea un nuevo ambiente asociando los símbolos a sus valores.
|#
(define (multi-extend-env ids exprs env)
  (if (= (length ids) (length exprs))
      (aEnv (make-immutable-hash (map cons ids exprs)) env)
      (error "wrong_input, mismatched lengths")))

#| EXTEND-ENV QUE RECIBE SIMBOLO Y VALOR -> MUTA el ambiente actual, no crea otro.
extend-frame-env!:: Sym Val Env -> Void
Agrega un nuevo par (Sym, Val) al ambiente usando mutación.
Este método no crea un nuevo ambiente.
|#
(define (extend-frame-env! id val env)
  (match env
    [(mtEnv) (aEnv (hash id val) env)]
    [(aEnv h rEnv) (def hupd (hash-set h id val))
                   (set-aEnv-hash! env hupd)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; parse :: s-expr -> Expr
(define (parse s-expr)
  (match s-expr
    [(this) this]
    [(? number?) (num s-expr)]
    [(? symbol?) (id s-expr)]
    [(? boolean?) (bool s-expr)]
    [(list '* l r) (binop * (parse l) (parse r))]
    [(list '+ l r) (binop + (parse l) (parse r))]
    [(list '- l r) (binop - (parse l) (parse r))]
    [(list '< l r) (binop < (parse l) (parse r))]
    [(list '= l r) (binop = (parse l) (parse r))]
    [(list 'or l r) (binop (λ (i d) (or i d)) (parse l) (parse r))]
    [(list 'and l r) (binop (λ (i d) (and i d)) (parse l) (parse r))]
    [(list 'not b) (unop not (parse b))]
    [(list 'if c t f) (my-if (parse c)
                             (parse t)
                             (parse f))]
    [(list 'seqn e1 e2) (seqn (parse e1) (parse e2))]
    [(list 'local (list e ...)  b)
     (lcal (map parse-def e) (parse b))] ;Hasta aqui estaba hecho antes de la tarea
    [(list 'object members ...) (object #f (map parse members))] ;El #f es por la delegacion
    [(list 'field id val) (field id (parse val))] ;Opcion a futuro, poner parse a id
    [(list 'method id params body) (method id params (parse body))] ;Opcion a futuro, poner parse a id y params
    [(list 'get id) (get id)] ;Opcion a futuro, poner parse a id
    [(list 'set id val) (set id (parse val))] ;Opcion a futuro, poner parse a id
    [(list 'send obj met) (send (parse obj) met (list (parse obj)))] ;Opcion a futuro, poner parse a met
    [(list 'send obj met params ...) (send (parse obj) met (cons (parse obj) (map parse params)))] ;Asume que params es lista
    [(list 'fun (list params ...) bdy) (object #f (list (method 'app params (parse bdy))))]
    [(list id val ...) (send (parse id) 'app (cons (parse id) (map parse val)))]
    ))


;; parse-def :: s-expr -> Def
(define (parse-def s-expr)
  (match s-expr
    [(list 'define id b) (my-def id (parse b))]))

;; interp :: Expr Env -> Val
(define (interp expr env)
  (match expr
    [(num n) (numV n)]
    [(bool b) (boolV b)]
    [(binop f l r) (make-val (f (open-val (interp l env))
                                (open-val (interp r env))))]
    [(unop f s) (make-val (f (open-val (interp s env))))]
    [(my-if c t f)
     (def (boolV cnd) (interp c env))
     (if cnd
         (interp t env)
         (interp f env))]
    [(id 'this) (void)]
    [(id x) (env-lookup x env)]
    [(seqn expr1 expr2) (begin
                          (interp expr1 env)
                          (interp expr2 env))]
    [(lcal defs body)
     (let ([new-env (multi-extend-env '() '() env)])
       (for-each (λ(x)
                   (def (cons id val) (interp-def x new-env))
                   (extend-frame-env! id val  new-env)
                   #t) defs)
       (interp body new-env))] ;Hasta aqui estaba hecho antes de la tarea
    [(object extension members) (if extension
                                    "Aqui poner para la p2"
                                    (for-each (λ (x) ;En interp-members pasa toda la magia
                                                (interp-members x env)
                                                #t) members))]
    [(get id)
     (printf "El ambiente es: ~a" env)
     (env-lookup id env)]
    [(set id val)
     ;(printf "El ambiente es ~a . ~n" env)
     (extend-frame-env! id (interp val env) env)]
    [(send (id obj) metodo params)
     (def (closureM met bdy mparam menv) (env-lookup metodo env))
     (define nenv (multi-extend-env mparam
                                    (map (λ (x) (interp x env)) (cdr params))
                                    menv))
     (interp bdy nenv)]
    [_ expr]
    ))


;; open-val :: Val -> Scheme Value
(define (open-val v)
  (match v
    [(numV n) n]
    [(boolV b) b]
    [(closureO membs env) (closureO membs env)]))

;; make-val :: Scheme Value -> Val
(define (make-val v)
  (match v
    [(? number?) (numV v)]
    [(? boolean?) (boolV v)]
    ))

;;interp-members :: Member  Env -> Expr
(define (interp-members a-member env)
  (match a-member
    [(field id val) (extend-frame-env! id (interp val env) env)]
    [(method id param body) (extend-frame-env! id (closureM id body param env) env)]
    [_ (error "Member is not method nor field: " a-member)]))


;; interp-def :: Def, Env -> Expr
(define (interp-def a-def env)
  (match a-def
    [(my-def id body) (cons id (interp body env))]))

;; run :: s-expr -> Val
(define (run s-expr)
  (interp (parse s-expr) empty-env))

#|
run-val:: s-expr -> Scheme-Val + Val
Versión alternativa de run, que retorna valores de scheme para primitivas y
valores de MiniScheme para clases y objetos
|#
(define (run-val s-expr)
  (define val (interp (parse s-expr) empty-env))
  (match val
    [(numV n) n]
    [(boolV b) b]
    [x x]))

