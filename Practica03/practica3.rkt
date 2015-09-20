#lang plai

(require "practica3-base.rkt")

#|
EJERCICIO 3
La función bmp->zone recibe:
 lista- una lista que contendrá dos enteros 
(frecuencias cardiacas)

listaF- una lista del tipo zones

Regresa las zonas de frecuencia cardiaca que estén dentro del rango
definido en 'lista'
|#

(define (bmp->zone lista listaF)
  (if (empty? lista)
      '();;regresa la lista vacía
       (revisar-rango lista listaF '(resting warm-up fat-burning aerobic maximum))))

#|
Entra a la lista si el minimo está dentro del rango
o el máximo está dentro del rango
|#

(define (revisar-rango lista listaF nombres) 
 (list 
  (cond
    [(empty? listaF) '()]
    ;[(and (<= (car lista) ((car nombres)-low (car listaF))) (<= ((car nombres)-low (car listaF))  (car (cdr lista)) ) ) ;; (min <= MINIMO) and (MINIMO <= max) para resting
    ; (car listaF)];;concatena el primer elemento de listaF
    ;[(and (<= (car lista) ((car nombres)-high (car listaF))) (<= ((car nombres)-high (car listaF))  (car (cdr lista)) ) );; (min <= MAXIMO) and (MAXIMO <= max) para resting
     ;(car listaF)];;concatena el primer elemento de listaF
    [revisar-rango lista (cdr listaF) (cdr nombres)]
   
   )))


;;Sección II

;;-----------------------------------------------------------------------------------------------------
;Dado un árbol de tipo BTree, determinar el número de nodos internos que tiene.
(define (ninBT btree)
  (cond
    [(EmptyBT? btree) 0]
    [(and (EmptyBT? (BNode-l btree)) (EmptyBT? (BNode-r btree)) )  0];es una hoja, así que no cuenta
    [else   (+ 1 (ninBT (BNode-l btree)) (ninBT (BNode-r btree)) )] ))

(test (ninBT (BNode < (BNode < (EmptyBT) 7 (EmptyBT)) 2 (BNode < (BNode < (EmptyBT) 11 (BNode < (EmptyBT) 00 (EmptyBT))) 3 (BNode < (EmptyBT) 33 (EmptyBT))))) 3)
(test (ninBT (EmptyBT)) 0)
(test (ninBT (BNode < (BNode < (EmptyBT) 3 (EmptyBT)) 1 (BNode < (EmptyBT) 2 (EmptyBT)))) 1)
(test (ninBT (BNode < (BNode < (BNode < (BNode < (EmptyBT) 12 (EmptyBT)) 7 (EmptyBT)) 3 (EmptyBT)) 1 (BNode < (EmptyBT) 2 (EmptyBT)))) 3)
(test (ninBT (BNode < (BNode < (EmptyBT) 7 (EmptyBT)) 2 (BNode < (EmptyBT) 3 (BNode < (EmptyBT) 33 (BNode < (EmptyBT) 4 (BNode < (EmptyBT) 1 (EmptyBT))))))) 4)

;;--------------------------------------------------------------------------------------------
;;Dado un árbol de tipo BTree Determina el número de hojas no vacías
(define (nlBT btree)
  (if (BTree? btree)
      (nlBT-aux btree );llamada a la función auxiliar
      ("ninBT Error, Expected BTree") ))

;;Función auxiliar para nlBT. Si ambas partes son vacías (EmptyBT) entonces estamos sobre una hoja
(define (nlBT-aux btree)
  (cond
    [(EmptyBT? btree) 0]
    [(and (EmptyBT? (BNode-l btree)) (EmptyBT? (BNode-r btree)) ) 1];es una hoja y sumamos 1
    [else (+ (+ 0 (nlBT-aux (BNode-l btree))) ;;lado izquierdo del árbol
             (+ 0 (nlBT-aux (BNode-r btree))) )] ))
  
(test (nlBT (BNode < (BNode < (EmptyBT) 7 (EmptyBT)) 2 (BNode < (BNode < (EmptyBT) 11 (EmptyBT)) 3 (BNode < (EmptyBT) 33 (EmptyBT))))) 3)
(test (nlBT (EmptyBT)) 0)
(test (nlBT (BNode < (EmptyBT) 2 (BNode < (EmptyBT) 3 (EmptyBT)))) 1)
(test (nlBT (BNode < (BNode < (EmptyBT) 3 (EmptyBT)) 1  (BNode < (EmptyBT) 3 (EmptyBT)))) 2)
(test (nlBT (BNode < (BNode < (EmptyBT) 3 (EmptyBT)) 2 (EmptyBT) )) 1)


;;----------------------------------------------------------------------------------------
;;Dado un árbol de tipo BTree Determina el número de nodos (sin contar hojas vacías)
(define (nnBT btree)
  (cond
    [(EmptyBT? btree) 0]
    [else  (+  (ninBT btree)
               (nlBT btree) ) ]))

(test (nnBT (BNode < (BNode < (EmptyBT) 7 (EmptyBT)) 2 (BNode < (BNode < (EmptyBT) 11 (EmptyBT)) 3 (BNode < (EmptyBT) 33 (EmptyBT))))) 5)
(test (nnBT (EmptyBT)) 0)
(test (nnBT (BNode < (BNode < (EmptyBT) 3 (EmptyBT)) 1 (BNode < (EmptyBT) 2 (EmptyBT)))) 3)
(test (nnBT (BNode < (EmptyBT) 3 (EmptyBT))) 1)
(test (nnBT (BNode < (EmptyBT) 3 (BNode < (EmptyBT) 7 (EmptyBT)))) 2)

;--------------------------------------------------------------------
;;mapBT - Dado una función de aridad 1 y un árbol de tipo BTree, aplicar la función sobre todos los valores de
;;los nodos del árbol (las funciones de aridad 1 sólo regresas números).
(define (mapBT f ab)
  (if(EmptyBT? ab)
     ab
     (BNode (BNode-c ab) (mapBT f (BNode-l ab)) (f(BNode-e ab)) (mapBT f (BNode-r ab)))))

(test (mapBT add1 (EmptyBT))(EmptyBT))
(test (mapBT add1 (BNode < (EmptyBT) 1 (BNode < (EmptyBT) 2 (EmptyBT))))(BNode < (EmptyBT) 2 (BNode < (EmptyBT) 3 (EmptyBT))))


;;Recorrido en preorden
(define (preorderBT t)
(type-case BTree t
  [EmptyBT () '() ]
  [BNode (c l e r)
       (append (list e)
               (preorderBT l)
               (preorderBT r))]))

;;el arbol base está definido en practica3-base
(test (preorderBT arbol-base) '("F" "B" "A" "D" "C" "E" "G" "I" "H"))

;;----------------------------------------------------------------------------------
;;Recorrido en inorder
(define (inorderBT t)
(type-case BTree t
  [EmptyBT () '() ]
  [BNode (c l e r)
       (append 
        (inorderBT l)
        (list e)
        (inorderBT r))]))

(test (inorderBT arbol-base) '("A" "B" "C" "D" "E" "F" "G" "H" "I"))

;;-------------------------------------------------------------------------
;;Recorrido en posorder
(define (posorderBT t)
(type-case BTree t
  [EmptyBT () '() ]
  [BNode (c l e r)
       (append 
        (posorderBT l)
        (posorderBT r)
        (list e))]))

(test (posorderBT arbol-base) '("A" "C" "E" "D" "B" "H" "I" "G" "F"))
