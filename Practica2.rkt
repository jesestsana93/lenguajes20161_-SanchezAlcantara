
#lang plai

#|
--------------------------------
|  Lenguajes de programación     |
|  Práctica 1                    |
|  Guerrero Chávez Diana Lucía   |
| Sánchez Alcántara Jesús Esteban|

---------------------------------

|#

;SECCION I

#|
1. Array - Definir un tipo de dato Array que tenga un constructor de tipo MArray. El entero sirve para definir
el tamaño del arreglo.
|#
(define-type Array
  [MArray (n number?)
            (l (listof number?))])

#|
2. List - Definir un tipo de dato recursivo llamado MList que tenga a la lista vacia MEmpty y el constructor de
tipo MCons.
|#
(define-type MList
  [MEmpty]
  [MCons (n number?)
         (rest MList?)])

#|
3. NTree - Definir un tipo de dato recursivo llamado NTree que tenga como una hoja nula TLEmpty y un
constructor de tipo NodeN (estos arboles son n-arios)
|#
(define-type NTree
  [TLEmpty]
  [NodeN (n number?)
         (l (listof NTree?))])


#|
EJERCICIO 4
Position
Tiene un constructor de tipo 2D-Point que toma los números reales
que indican una posición en el plano cartesiano
|#

(define-type Position
  [2D-Point (x number?) (y number?)];constructor n y m números reales
  )

(test (2D-Point 0 0) (2D-Point 0 0))
(test (2D-Point -1 33) (2D-Point -1 33))
(test (2D-Point 7.14 1) (2D-Point 7.14 1))
(test (2D-Point 6 6) (2D-Point 6 6))
(test (2D-Point 0 (+ 1 1)) (2D-Point 0 2))

#|
EJERCICIO 5
Figure
|#

(define-type Figure
  [Circle (centro Position?) (radio number?)]
  [Square (esquina Position?) (longitud number?)]
  [Rectangle (esquina Position?) (ancho number?) (largo number?)])

(test (Circle (2D-Point 5.5 7) 5) (Circle (2D-Point 5.5 7) 5))
(test (Square (2D-Point (+ 0 0) 11) 5) (Square (2D-Point 0 11) 5))
(test (Rectangle (2D-Point 1 1) 1 2) (Rectangle (2D-Point 1 1) 1 2))
(test (Rectangle (2D-Point -11 -2.2) 12 12) (Rectangle (2D-Point -11 -2.2) 12 12))
(test (Square (2D-Point 0 0) 10) (Square (2D-Point 0 0) 10))

;SECCION II

#|
6.setvalueA - Dado un arreglo de tipo Array, una posicion y un valor numerico v, regresar otro arreglo con
el valor v intercambiado en la posicion indicada del arreglo original. En caso de que la posicion sea igual o
mayor al tamaño especificado en el arreglo, regresar un error Out of bounds
|#
(define (coloca l pos v count)
  (cond
    [(< count pos) (cons (car l) (coloca (cdr l) pos v (add1 count)))]
    [(> count pos) empty]
    [(= count pos) (cons v (cdr l))]))

(define (setvalueA ar pos v)
  (if (not(Array? ar))
      (error 'setvalueA "Unknown Type")
      (if (> pos (- (MArray-n ar) 1))
          (error 'setvalueA "Out of bounds")
          (coloca (MArray-l ar) pos v 0))))
          
          
 #|
8.printML - Dada una lista de tipo MList, imprimirla en un formato legible. Puedes utilizar las funciones para
manipular cadenas y la funcion
|#
(define (printML ml)
  (if (MEmpty? ml)
      "[]"
      (if(MEmpty? (MCons-rest ml))
        (string-append "[" (~a (MCons-n ml)) "]")
         (string-append "[" (auxPrint ml) "]"))))
 
(define (auxPrint ml)
  (if (MEmpty? ml)
      ""
      (if(MEmpty? (MCons-rest ml))
         (string-append (~a (MCons-n ml)))
         (string-append (~a (MCons-n ml)) ", " (auxPrint (MCons-rest ml))))))

#|
9. concatML - Dadas dos listas de tipo MList, regresar la concatenacion
|#
(define (concatML ml1 ml2)
  (if(MEmpty? ml1)
     ml2
      (MCons (MCons-n ml1) (concatML (MCons-rest ml1) ml2))))

#|
10.lengthML - Dada una lista de tipo MLista, regresar la cantidad de elementos que tiene
|#
(define (lengthML ml)
  (if(MEmpty? ml)
     0
     (+ 1 (lengthML (MCons-rest ml)))))


 
#|
area
|#

(define (area figurita)
  (if (Figure? figurita)
      (cond ;revisamos que tipo de figura es
        [(Circle? figurita) (* pi (* (Circle-radio figurita) (Circle-radio figurita)))]
        [(Square? figurita) (* (Square-longitud figurita) (Square-longitud figurita) )]
        [(Rectangle? figurita) (* (Rectangle-ancho figurita) (Rectangle-largo figurita))])
      "Error. Se necesita un tipo Figure";no es del tipo Figure
      ))

(test (area (Circle (2D-Point 5.5 7) 5)) 78.53)
(test (area (Square (2D-Point (+ 0 0) 11) 12)) 144)
(test (area 12) "Error. Se necesita un tipo Figure")
(test (area (Rectangle (2D-Point -11 -2.2) 6 9)) 54)
(test (area 'x) "Error. Se necesita un tipo Figure")



;Ejercicio 18 
;in-figure?
;verifico que su distancia al centro sea menor al radio y en las otras hago un rango y verifico que estté en el rango
(define (infigure fig po)
  (type-case Figure fig
  [Circle (pos rad) (< (+ (*(-(2D-Point-x po)(2D-Point-x pos))(-(2D-Point-x po)(2D-Point-x pos))) (*(-(2D-Point-y po)(2D-Point-y pos))(-(2D-Point-y po)(2D-Point-y pos)))) (* rad rad))]
  [Square (pos lon) (and (and (< (2D-Point-x pos) (2D-Point-x po) ) (< (2D-Point-x po) (+(2D-Point-x po) lon)));alto
                         (and ( < (-(2D-Point-y pos) lon) (2D-Point-y po) ) (< (2D-Point-y po) (2D-Point-y pos))))];ancho
  [Rectangle (pos an lar) (and (and (< (2D-Point-x pos) (2D-Point-x po) ) (< (2D-Point-x po) (+(2D-Point-x po) lar)));largo x
                         (and ( < (-(2D-Point-y pos) an) (2D-Point-y po) ) (< (2D-Point-y po) (2D-Point-y pos))))]));ancho y

(test (infigure (Circle (2D-Point 0 0) 2) (2D-Point 0 0)) #t)
(test (infigure (Circle (2D-Point 0 0) 2) (2D-Point 0 1)) #t)
(test (infigure (Square (2D-Point 0 0) 2) (2D-Point 1 -1)) #t)
(test (infigure (Square (2D-Point 0 0) 2) (2D-Point 1 1)) #f)
(test (infigure (Circle (2D-Point 0 0) 2) (2D-Point 1 2)) #f)

#|
EJERCICIO 7
MArray2MList
|#

(define (MArray2MList lista)
  (if (MArray? lista)
      (if (empty? (MArray-l lista))
          (MEmpty)
          (MCons  (car (MArray-l lista)) (MArray2MList (MArray (MArray-n lista) (cdr (MArray-l lista))))))
    "Error. se esperaba un tipo MArray"))

(test (MArray2MList (MArray 0 '())) (MEmpty))
(test (MArray2MList (MArray 2 '(1 2))) (MCons 1 (MCons 2 (MEmpty))))
(test (MArray2MList (MArray 3 '(1 2 4))) (MCons 1 (MCons 2 (MCons 4 (MEmpty)))))
(test (MArray2MList (MArray 1 '(1))) (MCons 1 (MEmpty)))
(test (MArray2MList '(22 3)) "Error. se esperaba un tipo MArray")


#|
EJERCICIO 11
mapML
|#

(define (mapML lista funcion)
  (if (MList? lista)
      (if (MEmpty?  lista)
          (MEmpty)
          (MCons [funcion (MCons-n lista)] ;obtenemos el numero y le aplicamos la funcion
                 (mapML [MCons-rest lista] funcion)))
    "Error. se esperaba un tipo MArray"))

;(define (add1 n) (+ 1 n))

(test (mapML (MCons 1 (MCons 2 (MEmpty))) add1) (MCons 2 (MCons 3 (MEmpty))))
(test (mapML (MEmpty) add1) (MEmpty))
(test (mapML (MCons 10 (MCons 3 (MEmpty))) (lambda (x) (* x x))) (MCons 100 (MCons 9 (MEmpty))))
(test (mapML '(3 5 6) add1) "Error. se esperaba un tipo MArray")
(test (mapML (MCons 3 (MCons 6 (MEmpty))) (lambda (x) (* x 2))) (MCons 6 (MCons 12 (MEmpty))))
