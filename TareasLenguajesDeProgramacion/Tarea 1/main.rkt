#lang play
(require "machine.rkt")
(print-only-errors #t)
;;;;;;;;;;;;;;;;;;;;;;;
;; Language definition
;;;;;;;;;;;;;;;;;;;;;;;

#|
<s-expr> ::= <num>
         | <id>
         | {+ <s-expr> <s-expr>}
         | {- <s-expr> <s-expr>}
         | {with {<s-expr> : <type> <s-expr>} <s-expr>}
         | {fun {<id>  : <type>} [: <type>] <expr>}
         | {<expr> <expr>}

<type> ::= Num
         | {<type> -> <type>}
|#
(deftype Expr
  (num n)
  (add l r)
  (sub l r)
  (id s)
  (fun id targ body tbody) ;el tbody es el parametro opcional
  (fun-db body)
  (acc n) ;Se usa para la pregunta 3
  (app fun-id arg-expr))

(deftype Type
  (TNum)
  (TFun arg ret))

#|
1. Funciones de primera clase con tipos declarados
|#

;parse-type :: <type> -> Type
;Given a Racket type, returns a type we can work with
(define (parse-type type)
  (match type
    ['Num (TNum)]
    [(list 'Num) (TNum)]
    [(list l arrow r) (TFun (parse-type l) (parse-type r))]
    [_ (error "Parse error")]))

;parse :: <s-expr> -> Expr
;Given an <s-expr> returns the equivalent expresion which we can work
(define (parse s-expr)
  (match s-expr
    [(? number?) (num s-expr)]
    [(? symbol?) (id s-expr)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'with (list i puntos tbody targ) body)
     (app (fun i (parse-type tbody) (parse body) #f)(parse targ))]
    [(list 'fun (list i puntos target) puntos2 tbody body)
     (fun i (parse-type target) (parse body) (parse-type tbody))]
    [(list 'fun (list i puntos target) body)
     (fun i (parse-type target) (parse body) #f)]
    [(list f e) (app (parse f) (parse e))]
    ))

;prettify :: Type -> <type>
;Given a type we can work it, returns something the user can understand (racket types)
(define (prettify type)
  (match type
    [(? TNum?) 'Num]
    [(TFun arg1 arg2) (list (prettify arg1) '-> (prettify arg2))]))

#|
2. Verificación de Tipos
|#

; The environments were obtained from the lessons files
; ambientes: Env
; tipo de dato abstracto
; (= name + operations)
; Env
; empty-env :: Env
; extend-env :: Id x Val x Env -> Env
; lookup-env :: Id x Env -> Val (o free identifier error)
(deftype Env
  (mtEnv)
  (aEnv id val next))

(define empty-env mtEnv)
(define extend-env aEnv)
(define (lookup-env id env)
  (match env
    [(mtEnv) (error "Type error: free identifier:" id)]
    [(aEnv x v n) (if (equal? x id)
                      v
                      (lookup-env id n))]))


;typeof :: expr x [env] -> Type (or Type Expresions)
;Given a Expr, returns the resultant type of the expresion.
;   if something is not good with the argument, returns a modified error message.
#|
-- al typeof entra algo de la forma que retorna el parse --
> (parse '{fun {x : Num} : Num 5})   ;FUNCION CON TIPO DE RETORNO ANOTADO
(fun 'x (TNum) (num 5) (TNum))
--> aplicarle typeof queda (TFun (TNum) (TNum))
|#
(define (typeof expr [env (empty-env)])
  (match expr
    [(num n) (TNum)]
    [(add l r) (if (equal? (typeof l env) (TNum))
                   (if (equal? (typeof r env) (TNum))
                       (TNum)
                       ((error (type-error "+" "2" (TNum) (typeof r env)))))
                   ((error (type-error "+" "1" (TNum) (typeof r env)))))]
    [(sub l r) (if (equal? (typeof l env) (TNum))
                   (if (equal? (typeof r env) (TNum))
                       (TNum)
                       ((error (type-error "-" "2" (TNum) (typeof r env)))))
                   ((error (type-error "-" "1" (TNum) (typeof r env)))))]
    [(id x) (lookup-env x env)]
    [(fun id tipoParametro cuerpo tipoRetorno)
     (if tipoRetorno
         (if (equal? tipoRetorno (typeof cuerpo (extend-env id tipoParametro env)))
             (TFun tipoParametro tipoRetorno)
             ((error (type-error "fun" "1" tipoRetorno (typeof cuerpo (extend-env id tipoParametro env))))))
         (TFun tipoParametro (typeof cuerpo (extend-env id tipoParametro env))))]
    [(app funcion argumento)
     (match funcion
       [(fun idFun tipoParametro cuerpo tipoRetorno)
        (if (equal? tipoParametro (typeof argumento (extend-env idFun tipoParametro env)))
            (match (typeof funcion env)
              [(TFun targ tret) tret])
            (error (type-error "app" "2" tipoParametro (typeof argumento (extend-env idFun tipoParametro env)))))]
       [_ (error (type-error "app" "1" (list "T" "S") (typeof funcion)))])]))


;typecheck :: <s-expr> -> <type>
;Given an <s-expr>, returns the resultant type expresion of it
;    uses the function typeof, so it has the same modified error messages
(define (typecheck s-expr)
  (prettify (typeof (parse s-expr))))


;Aqui abajo funciones aux

;typeAString :: Type -> String
;Given a Type, it returns the equivalent string to print or send errors
;tests are included in the typeof function tests.
(define (typeAString type)
  (match type
    ["T" "?T"]
    ["S" "?S"]
    [(TNum) "Num"]
    [(TFun targ tret) (string-append "(" (typeAString targ) " -> " (typeAString tret) ")")]
    [(list t s) (string-append "(" (typeAString t) " -> " (typeAString s) ")")]
    [_ ("Unknown Type")]))

;type-error Expr x String x Expr x Expr -> String
;Given a series of arguments, make a modified message in the required format for errors
;tests are included in the typeof function tests.
(define (type-error exprMala posicion esperado encontrado)
  (format "Type error in expression ~a position ~a : expected ~a found ~a" exprMala posicion (typeAString esperado) (typeAString encontrado)))


#|
3. Compilación
|#

; lookup-stack :: stack x <id> -> int (error)
; Given a stack and an id, returns the index of the position of the id in the stack
;    if the indes is not in the stack, throws and error
;tests are included in the deBruijn tests.
(define (lookup-stack stack id [i 0])
  (match stack
    [(EmptyStack) (error "Free identifier:" id)]
    [(Stacked v n) (if (equal? v id)
                       i
                       (lookup-stack (stack-pop stack) id (+ i 1)))]))

;deBruijn :: Expr x [stack] -> Expr
;given an expresion, returns the same but using deBruijn notation
(define (deBruijn expr [stack (EmptyStack)])
  (match expr
    [(num n) (num n)]
    [(add l r) (add (deBruijn l stack) (deBruijn r stack))]
    [(sub l r) (sub (deBruijn l stack) (deBruijn r stack))]
    [(id x) (acc (lookup-stack stack x))]
    [(fun id tipoArg body tipoRet)
     (fun-db (deBruijn body (stack-push stack id)))]
    [(app fun-id arg)
     (app (deBruijn fun-id stack) (deBruijn arg stack))]))

;compile :: Expr -> SECD instructions
;Given an Expresion with the deBruijn notation, returns an exprsion with SECD instruction set
(define (compile expr)
  (match expr
    [(num n) (INT-CONST n)]
    [(add l r) (flatten (list (compile r) (compile l) (ADD)))]
    [(sub l r) (flatten (list (compile r) (compile l) (SUB)))]
    [(acc x) (ACCESS x)]
    [(fun-db body) (CLOSURE (flatten (list (compile body) (RETURN))))]
    [(app fun-id arg-expr)
     (flatten (list (compile arg-expr) (compile fun-id) (APPLY)))]))

;typed-compile :: <s-expr> -> SECD
;Given an s-expr, returns the machine code for it
(define (typed-compile s-expr)
  (define parsedExpr (parse s-expr))
  (define validateExpr (typeof parsedExpr))
  (define deBruijnExpr (deBruijn parsedExpr))
  compile (deBruijnExpr))

;Every main function has tests on the test.rkt file.