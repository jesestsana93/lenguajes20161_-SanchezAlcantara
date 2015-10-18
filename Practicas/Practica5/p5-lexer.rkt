#lang plai

;(require "p4-base.rkt")

;; B::= {|}|<number>|<symbol>
;; toks listof(<char>) listof(<char>) -> listof(B)
;; Toma una cadena de caractéres y genera una lista de llaves que abren, cierran, números
;; y símbolos.
;; (define (toks l acc)
(define (toks l acc)
  (if (empty? l)
      (if (empty? acc)
          empty
          (let* ((taux (list->string (reverse acc)))
                 (nm (string->number taux))
                 (sm (string->symbol taux))
                 (t (if nm nm sm)))
            t))
      (let* ((c (car l))) 
        (cond
          ((or (eq? c #\{)
               (eq? c #\})) (if (empty? acc)
                                (cons c (toks (cdr l) empty))
                                (let* ((taux (list->string (reverse acc)))
                                       (nm (string->number taux))
                                       (sm (string->symbol taux))
                                       (t (if nm nm sm)))
                                  (cons t (cons c (toks (cdr l) empty))))))
          ((eq? c #\ ) (if (empty? acc)
                           (toks (cdr l) empty)
                           (let* ((taux (list->string (reverse acc)))
                                  (nm (string->number taux))
                                  (sm (string->symbol taux))
                                  (t (if nm nm sm)))
                             (cons t (toks (cdr l) empty)))))
          (else (toks (cdr l) (cons c acc)))))))

;; A::= <number>|<symbol>|listof(<A>)
;; B::= {|}|<number>|<symbol>|
;; until-next-end-brace: listof(B) -> (listof(A), listof(B))
;; Determina los numeros, símbolos o listas hasta la llave que cierra al ambiente
;; dada una lista de llaves que abren, cierran, números y símbolos.
(define (until-next-end-brace l)
  (if (empty? l)
      (list empty empty)
      (let ((t (car l))) 
        (cond
          ((eq? t #\{) (let* ((ds (until-next-end-brace (cdr l)))
                              (tks (car ds))
                              (rem (cadr ds))) 
                         (if (empty? rem)
                             tks
                             (let* ((new-ds (until-next-end-brace rem))
                                    (new-tks (car new-ds))
                                    (new-rem (cadr new-ds)))
                               (list (cons tks new-tks) new-rem)))))
          ((eq? t #\}) (list empty (cdr l)))
          (else (let* ((ds (until-next-end-brace (cdr l)))
                       (tks (car ds))
                       (rem (cadr ds)))
                  (list (cons t tks) rem)))))))

;; A::= <number>|<symbol>|listof(<A>)
;; lexer: <string> -> A
;; Toma una cadena de caractéres y genera una expresión en sintaxis concreta.
;; (define (lexer s)
(define (lexer s)
  (let ((ts (toks (string->list s) empty)))
    (if (list? ts)
        (until-next-end-brace ts)
        ts)))
