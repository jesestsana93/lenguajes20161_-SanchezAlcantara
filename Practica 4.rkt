#lang plai
(require "practica4-base.rkt")

#|
--------------------------------
|  Lenguajes de programación     |
|  Práctica 1                    |
|  Guerrero Chávez Diana Lucía   |
| Lázaro Arias Jorge Alberto     |
| Sánchez Alcántara Jesús Esteban|
---------------------------------
|#

#|
EJERCICIO 1
desugar Define una función que toma una expresión en sintaxis FAES y que regresa una expresión en
sintaxis FAE.
|#
(define (desugar expr)
  (type-case FAES expr
    [numS (n) (num n)]
    [addS (l r) (add (desugar l)
                     (desugar r))]
    [subS (l r) (sub (desugar l)
                     (desugar r))]
    [withS (id named body) (app (fun id (desugar body))
                                (desugar named))]
    [idS (s) (id s)]
    [funS (p b) (fun p (desugar b))]
    [appS (f e) (app (desugar f)
                     (desugar e))]))
