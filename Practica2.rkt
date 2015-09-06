
#lang plai

#|
--------------------------------
|  Lenguajes de programación     |
|  Práctica 1                    |
|  Guerrero Chávez Diana Lucía   |
| Sánchez Alcántara Jesús Esteban|

---------------------------------

|#



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
