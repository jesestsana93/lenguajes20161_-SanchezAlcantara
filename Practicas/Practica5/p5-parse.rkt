#lang plai
(define (any? x) #t)
;; CFWAE con multi-with, multi-rec.
;;
;; <CFWAE>::= <num>
;;          | <bool>
;;          | <char>
;;          | <string>
;;          | <id>
;;          | {with {{<id> : <type> <CFWAE>}+} <CFWAE>}
;;          | {fun {{<id> : <type>}+} : <type> <CFWAE>}
;;          | {if <CFWAE> <CFWAE> <CFWAE>}
;;          | {equal? <CFWAE> <CFWAE>}
;;          | {<op> <CFWAE>}
;;          | {<binop> <CFWAE> <CFWAE>}
;;          | {<CFWAE> <CFWAE>*}
;; 
;; <op>::= inc | dec | zero? | neg | string-length
;; 
;; <binop>::= + | - | * | / | < | > | <= | >= | and | or | string-append | equal? 
;;
;; <type>::= number | boolean | char | string | ([<type> ->]* <type>)
(define-type CFWAE
  [id (name symbol?)]
  [num (n number?)]
  [bool (b boolean?)]
  [with (tbs (listof TypedBinding?)) (body CFWAE?)]
  [fun (tpfs (listof TypedSymbol?)) (body CFWAE?)]
  [app (func CFWAE?) (prs (listof CFWAE?))]
  [op (fid id?) (r CFWAE?)]
  [binop (fid id?) (l CFWAE?) (r CFWAE?)])

;; Definimos el data-type TypeBinding.
(define-type TypedBinding
  (tbind)) ; ... )) ¿Qué parámetros necesitamos en tbind?

;; Definimos el data-type TypedSymbol
(define-type TypedSymbol
  (tsym)) ; ... )) ¿Qué parámetros necesitamos en tsym?

;; Definimos el data-type Type
;(define-type Type ...)

; FUNCIONES RFWAE

;; A::= <number>|<symbol>|listof(<A>)
;; parse: A -> RFWAE
;; Toma una expresión en sintaxis concreta y genera el árbol de sintaxis abstracta.
;; (define (parse sexp)
(define (parse sexp)
  (cond
    [(symbol? sexp) (id sexp)]
    [(number? sexp) (num sexp)]
    [(list? sexp)
     (case (car sexp)
       [(with) (with (parse-tbindings (cadr sexp)) (parse (caddr sexp)))]
       [(fun) (fun (cadr sexp) (parse (caddr sexp)))]
       [(inc dec zero? num? neg bool? char? string-length string?) 
        (op (id (car sexp)) (parse (second sexp)))] ;; ¡OJO! Encerramos el símbolo de operación en un (id ...)
       [(+ - * / < > <= >= and or string-append equal?) 
        (binop (id (car sexp)) (parse (second sexp)) (parse (third sexp)))] ;; ¡OJO! Encerramos el símbolo de operación en un (id ...)
       [else (app (parse (car sexp)) (map parse (cdr sexp)))])]))

; FUNCIONES AUXILIARES

;; A::= <number>|<symbol>|listof(<A>)
;; B::= (list <symbol> <A>)
;; parse-bindings: listof(B) -> listof(bind?)
;; "Parsea" la lista de bindings lst en sintaxis concreta
;; mientras revisa la lista de id's en busca de repetidos.
;; (define (parse-bindings lst) 
(define (parse-tbindings lst)
  (let ([bind-rep (busca-repetido lst (lambda (e1 e2) (symbol=? (car e1) (car e2))))]) ;; ¡Ojo! Corrige con respecto a la grámatica
    (if (boolean? bind-rep)
        (map (lambda (b) (tbind (car b) (parse (cadr b)))) lst) ;; ¡Ojo! Corrige con respecto a la grámatica
        (error 'parse-bindings (string-append "El id " (symbol->string (car bind-rep)) " está repetido")))))
  
;; busca-repetido: listof(X) (X X -> boolean) -> X
;; Dada una lista, busca repeticiones dentro de la misma
;; usando el criterio comp. Regresa el primer elemento repetido
;; o falso eoc.
;; (define (busca-repetido l comp) 
(define (busca-repetido l comp) 
  (cond
    [(empty? l) #f]
    [(member? (car l) (cdr l) comp) (car l)]
    [else (busca-repetido (cdr l) comp)]))

;; member?: X listof(Y) (X Y -> boolean) -> boolean
;; Determina si x está en l usando "c" para
;; comparar x con cada elemento de la lista.
;; (define (member? x l comparador)
(define (member? x l c)
  (cond
    [(empty? l) #f]
    [(c (car l) x) #t]
    [else (member? x (cdr l) c)]))
