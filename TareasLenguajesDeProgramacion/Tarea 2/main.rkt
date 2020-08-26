#lang play

;Structs en minischeme -> deftype

;README
;Les ahorro un poco de correccion: No alcanze a tocar nada del lazy, la p3 de listas pasa algunos
;tests, no todos, falla en la recursion, pero con listas simples pasa.
;El warmup y las otras preguntas de listas pasan todo creo, nota tentativa: 2.8

#|
<expr> ::= <num>
         | <bool>
         | <id>
         | <string>
         | {if <expr> <expr> <expr>}
         | {fun {<id>*}}  <expr>}
         | {<expr> <expr>*}
         | {local {<def>*} <expr>}
         | {match <expr> <case>+}

<case> ::= {'case <pattern> '=> <expr>}
<pattern> ::= <num>
         | <bool>
         | <string>
         | <id>
         | (<constr-id> <attr-id>*)

<def>  ::= {define <id> <expr>}
         | {datatype <typename> <type-constructor>*}}


<type-constructor> ::= {<id> <member>*}
<constr-id> :: = <id>
<attr-id> :: = <id>
<typename> :: = <id>
<member>   :: = <id>

|#
; expresiones
(deftype Expr
  (num n)
  (bool b)
  (str s)
  (ifc c t f)
  (id s)
  (app fun-expr arg-expr-list)
  (prim-app name args)   ; aplicación de primitivas
  (fun id body)
  (lcal defs body)
  (mtch val cases))

; definiciones
(deftype Def
  (dfine name val-expr) ; define
  (datatype name variants)) ; datatype

; variantes
(deftype Variant
  (variant name params))

; estructuras de datos
(deftype Struct
  (structV name variant values))

; caso en pattern matching
(deftype Case
  (cse pattern body))

; patrón
(deftype Pattern
  (idP id) ; identificador
  (litP l) ; valor literal
  (constrP ctr patterns)) ; constructor y sub-patrones

;; parse :: s-expr -> Expr
(define(parse s-expr)
  (match s-expr
    [(? number?) (num s-expr)]
    [(? boolean?) (bool s-expr)]
    [(? string?) (str s-expr)]
    [(? symbol?) (id s-expr)]
    [(list 'if c t f) (ifc (parse c) (parse t) (parse f))]
    [(list 'fun xs b) (fun xs (parse b))]
    [(list 'with (list (list x e) ...) b)
     (app (fun x (parse b)) (map parse e))]
    [(list 'local defs body)
     (lcal (map parse-def defs) (parse body))]
    [(list 'match val-expr cases ...) ; note the elipsis to match n elements
     (mtch (parse val-expr) (map parse-case cases))] ; cases is a list
    [(list 'list args ...)
     (define Empty (app (id 'Empty) '()))
     (define parsedList (append (map parse args) (cons Empty '())))
     (define (adapter l)
       (if (equal? (car l) Empty)
           Empty
           (app (id 'Cons) (list (car l) (adapter (cdr l))))))
     (adapter parsedList)]; Aqui entra (parse '{list {+ 1 1} 4 6})
    [(list f args ...) ; same here
     (if (assq f *primitives*)
         (prim-app f (map parse args)) ; args is a list
         (app (parse f) (map parse args)))]))
#|
(run '{match {list 2 {list 4 5} 6}
          {case {list a {list b c} d} => c}})

-> (parse '{match {list 2 {list 4 5} 6}
              {case {list a {list b c} d} => c}})

-> entra al caso del parse 'match
-> val expr := {list 2 {list 4 5} 6}
-> cases := {case {list a {list b c} d} => c}

+-> El parse del val expr es : (app (num 2) (list (app (num 4) (list (num 5))) (num 6)))
*A los cases se les aplica (map parse-case cases)
-> (parse-case {case {list a {list b c} d} => c})
-> pattern := {list a {list b c} d}
-> body := c

*Al pattern se le aplica (parse-pattern)
-> (parse-pattern {list a {list b c} d})

|#
; parse-def :: s-expr -> Def
(define(parse-def s-expr)
  (match s-expr
    [(list 'define id val-expr) (dfine id (parse val-expr))]
    [(list 'datatype name variants ...) (datatype name (map parse-variant variants))]))

; parse-variant :: sexpr -> Variant
(define(parse-variant v)
  (match v
    [(list name params ...) (variant name params)]))

; parse-case :: sexpr -> Case
(define(parse-case c)
  (match c
    [(list 'case pattern => body) (cse (parse-pattern pattern) (parse body))]))

;Poner esto en la terminal sirve para tener una idea del parse con match
#;(parse '{match {list}
          {case {Cons h r} => h}
          {case _ => 0}})

; parse-pattern :: sexpr -> Pattern
(define(parse-pattern p) ;{list a {list b c} d} esto es "pattern" de la 3 de listas
  (match p
    [(? symbol?)  (idP p)]
    [(? number?)  (litP (num p))]
    [(? boolean?) (litP (bool p))]
    [(? string?)  (litP (str p))]
    #;[(list 'list args ...) (constrP 'Cons (map parse-pattern args))]
    [(list 'list) (list 'Empty)]
    [(list 'list args ...)
     (constrP 'Cons (map parse-pattern (append args (cons 'Empty '()))))]
    #;[(list 'list args ...) (print (first p))]
    #;[(list 'list args ...) (print (car args))]
    #;[(list 'list args ...) (parse-pattern (lista-convertor p))]
    [(list ctr patterns ...) (constrP (first p) (map parse-pattern patterns))] ;(first p) -> 'Cons
    #;[(list ctr patterns ...) (print (patterns))]))


(define (lista-convertor lista)
  (if (equal? lista '())
      (app (id 'Empty) '())
      (app (id 'Cons) (list (car lista) (lista-convertor (cdr lista))))))
      

#| '(a) -> Es lo que entra como args, (car args) -> 'a, (cdr args) -> '()
(mtch
 (app (id 'Cons) (list (num 2) (app (id 'Empty) '())))
 (list (cse #<void> (id 'a))))

(mtch
 (app (id 'Cons) (list (num 2) (app (id 'Empty) '())))
 (list (cse (constrP 'Cons (list (idP 'a))) (id 'a))))
|#


#| ESTO TIENE QUE DAR EL PARSE DE LO DEL ENUNCIADO
(mtch
 (app
  (id 'Cons)
  (list
   (num 2)
   (app
    (id 'Cons)
    (list
     (app (id 'Cons) (list (num 4) (app (id 'Cons) (list (num 5) (app (id 'Empty) '())))))
     (app (id 'Cons) (list (num 6) (app (id 'Empty) '())))))))
 (list
  (cse
   (constrP
    'Cons
    (list
     (idP 'a)
     (constrP
      'Cons
      (list
       (constrP 'Cons (list (idP 'b) (constrP 'Cons (list (idP 'c) (constrP 'Empty '())))))
       (constrP 'Cons (list (idP 'd) (constrP 'Empty '())))))))
   (id 'c))))

   ESTO ES LO QUE ME ESTA DANDO
(mtch
 (app
  (id 'Cons)
  (list
   (num 2)
   (app
    (id 'Cons)
    (list
     (app
      (id 'Cons)
      (list
       (num 4)
       (app (id 'Cons) (list (num 5) (app (id 'Empty) '())))))
     (app (id 'Cons) (list (num 6) (app (id 'Empty) '())))))))
 (list
  (cse
   (constrP
    'Cons
    (list
     (idP 'a)
     (constrP 'Cons (list (idP 'b) (idP 'c)))
     (idP 'd)))
   (id 'c))))
|#


#|
Para el lazy, en clases se definen una nueva estructura exprV, que es una expresion y su ambiente.
Despues de esto hacen una funcion Strict, que asegura que los interp tienen que devolver si o si
  un valor con el que ya podamos trabajar, en clases son numV y closureV, entonces hace que si
  strict recibe un numV o closureV, lo pasa nomas, pero que si recibe una promesa, la interpreta (con un strict tambien)
Para asegurar el call-by-need, se usa mutacion. Las promesas tienen expr, env y un cache, que es una box
  que puede tener valor #f si no se ha evaluado o una estructura conocida si se evaluo. El if que ve esto
  se pone en la funcion strict. Lo que hacen: (if cache) -> pasa valor, else -> calcula y set-box al cache.
|#
;; interp :: Expr Env -> number/boolean/procedure/Struct
(define(interp expr env)
  (match expr
    ; literals
    [(num n) n]
    [(bool b) b]
    [(str s) s]
    ; conditional
    [(ifc c t f)
     (if (interp c env)
         (interp t env)
         (interp f env))]
    ; identifier
    [(id x) (env-lookup x env)]
    ; function (notice the meta interpretation)
    [(fun ids body)
     (λ (arg-vals)
       (interp body (extend-env ids arg-vals env)))]
    ; application
    [(app fun-expr arg-expr-list)
     ((interp fun-expr env)
      (map (λ (a) (interp a env)) arg-expr-list))] ;En el map, el (interp a env) hace eager eval.
    ; primitive application
    [(prim-app prim arg-expr-list)
     (apply (cadr (assq prim *primitives*))
            (map (λ (a) (interp a env)) arg-expr-list))]
    ; local definitions
    [(lcal defs body)
     (def new-env (extend-env '() '() env))
     (for-each (λ (d) (interp-def d new-env)) defs)
     (interp body new-env)]
    ; pattern matching
    [(mtch expr cases)
     (def value-matched (interp expr env))
     (def (cons alist body) (find-first-matching-case value-matched cases))
     (interp body (extend-env (map car alist) (map cdr alist) env))]))

; interp-def :: Def Env -> Void
(define(interp-def d env)
  (match d
    [(dfine id val-expr)
     (update-env! id (interp val-expr env) env)]
    [(datatype name variants)
     ;; extend environment with new definitions corresponding to the datatype
     (interp-datatype name env)
     (for-each (λ (v) (interp-variant name v env)) variants)]))

; interp-datatype :: String Env -> Void
(define(interp-datatype name env)
  ; datatype predicate, eg. Nat?
  (update-env! (string->symbol (string-append (symbol->string name) "?"))
               (λ (v) (symbol=? (structV-name (first v)) name))
               env))

; interp-variant :: String String Env -> Void
(define(interp-variant name var env)
  ;; name of the variant or dataconstructor
  (def varname (variant-name var))
  ;; variant data constructor, eg. Zero, Succ
  (update-env! varname
               (λ (args) (structV name varname args))
               env)
  ;; variant predicate, eg. Zero?, Succ?
  (update-env! (string->symbol (string-append (symbol->string varname) "?"))
               (λ (v) (symbol=? (structV-variant (first v)) varname))
               env))

;;;;; pattern matcher
(define(find-first-matching-case value cases)
  (match cases
    [(list) #f]
    [(cons (cse pattern body) cs)
     (let [(r (match-pattern-with-value pattern value))]
       (if (foldl (λ (x y)(and x y)) #t r)
           (cons r body)
           (find-first-matching-case value cs)))]))

(define(match-pattern-with-value pattern value)
  (match/values (values pattern value)
                [((idP i) v) (list (cons i v))]
                [((litP (bool v)) b)
                 (if (equal? v b) (list) (list #f))]
                [((litP (num v)) n)
                 (if (equal? v n) (list) (list #f))]
                [((constrP ctr patterns) (structV _ ctr-name str-values))
                 (if (symbol=? ctr ctr-name)
                     (apply append (map match-pattern-with-value
                                        patterns str-values))
                     (list #f))]
                [(x y) (error "Match failure")]))

;; run :: s-expr -> number/boolean/procedura/struct
(define(run prog)
  (define ambiente (extend-env '() '() (mtEnv)))
  
  (define Lists (interp-def (parse-def '{datatype List
                                                  {Empty}
                                                  {Cons val List}}) ambiente))
  
  (define Length (interp-def (parse-def '{define length {fun {l}
                                                             {match l
                                                               {case {Empty} => 0}
                                                               {case {Cons val next} => {+ 1 {length next}}}}}}) ambiente))
  
  (define output (interp (parse prog) ambiente)) ;Me defino una variable output.
  (match output ;Matcheo output y aplico pretty-printing o no
    [(? Struct?) (pretty-printing output)]
    [_ output]))

#|-----------------------------
Environment abstract data type
empty-env   :: Env
env-lookup  :: Sym Env -> Val
extend-env  :: List[Sym] List[Val] Env -> Env
update-env! :: Sym Val Env -> Void
|#
(deftype Env
  (mtEnv)
  (aEnv bindings rest)) ; bindings is a list of pairs (id . val)

(def empty-env  (mtEnv))

(define(env-lookup id env)
  (match env
    [(mtEnv) (error 'env-lookup "no binding for identifier: ~a" id)]
    [(aEnv bindings rest)
     (def binding (assoc id bindings))
     (if binding
         (cdr binding)
         (env-lookup id rest))]))

(define (extend-env ids vals env)
  (aEnv (map cons ids vals) ; zip to get list of pairs (id . val)
        env))

;; imperative update of env, adding/overriding the binding for id.
(define(update-env! id val env)
  (set-aEnv-bindings! env (cons (cons id val) (aEnv-bindings env))))

;;;;;;;

;;; primitives
; http://pleiad.cl/teaching/primitivas
(define *primitives*
  `((+       ,(lambda args (apply + args)))
    (-       ,(lambda args (apply - args)))
    (*       ,(lambda args (apply * args)))
    (%       ,(lambda args (apply modulo args)))
    (odd?    ,(lambda args (apply odd? args)))
    (even?   ,(lambda args (apply even? args)))
    (/       ,(lambda args (apply / args)))
    (=       ,(lambda args (apply = args)))
    (<       ,(lambda args (apply < args)))
    (<=      ,(lambda args (apply <= args)))
    (>       ,(lambda args (apply > args)))
    (>=      ,(lambda args (apply >= args)))
    (zero?   ,(lambda args (apply zero? args)))
    (not     ,(lambda args (apply not args)))
    (and     ,(lambda args (apply (lambda (x y) (and x y)) args)))
    (or      ,(lambda args (apply (lambda (x y) (or x y)) args)))))


;Aqui voy a partir definiendo las funciones de la tarea

; pretty-string :: Struct -> String
(define (pretty-printing struct [deep 0])
  (match struct
    [(structV type head val) (if (equal? type 'Nat)
                                 (if (not (equal? head 'Zero))
                                     (format "{~a ~a}" head (pretty-printing (first val)))
                                     "{Zero}")
                                 (if (equal? type 'List)
                                     (if (equal? head 'Cons)
                                         (if (equal? deep 0)
                                             (format "{list ~a~a}" (first val) (pretty-printing (second val) (+ deep 1)))
                                             (format " ~a~a" (first val) (pretty-printing (second val) (+ deep 1))))
                                         (if (equal? deep 0)
                                             "list"
                                             ""))
                                     (print "Tipo no es List ni Nat")))]
                                     
    [_ (print struct)]
    ;Si esto falla en algun lugar por que el run no devuelve struct, agregarle un caso _
    ))