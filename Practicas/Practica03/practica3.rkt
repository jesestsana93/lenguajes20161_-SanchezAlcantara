#lang plai

(require "practica3-base.rkt")

#|
EJERCICIO 1

La funcion zones recibe:
minimo- ritmo cardiaco de descanso de una persona
maximo- el maximo ritmo cardiaco de una persona

Regresa una lista de zonas de frecuencia cardiaca

Formulas (con cambios para que coincida con el resultado esperado)
rest + range * (0.5 + (0.1*(i-1))) (minimo)
rest + range * (0.5 + (0.1*i))-1 (maximo)

|#


(define (zones rest max)
  (let ([range (- max rest)]);;definimos una variable que se llama 'range'
    (list
   (resting rest (+ (- (* range 0.5) 1) rest))
   (warm-up (+ rest (* range (+ 0.5 0)))
            (- (+ rest (* range (+ 0.5 0.1))) 1))
   (fat-burning (+ rest (* range (+ 0.5 0.1)))
            (- (+ rest (* range (+ 0.5 0.2))) 1))
   (aerobic (+ rest (* range (+ 0.5 0.2)))
            (- (+ rest (* range (+ 0.5 0.3))) 1))
   (anaerobic (+ rest (* range (+ 0.5 0.3)))
            (- (+ rest (* range (+ 0.5 0.4))) 1))
   (maximum (+ rest (* range (+ 0.5 0.4)))
            (+ rest range))
   )))


#|
EJERCICIO 2

La función get-zone recibe:
nombre- un simbolo que representa el nombre de la zona
lst- una lista del tipo zones

llama a la función get-zone-aux 
|#

(define (get-zone nombre lst)
  (get-zone-aux nombre lst '(resting warm-up fat-burning aerobic anaerobic maximum))
  )

#|
La función get-zone-aux recibe:
nombre- un simbolo que representa el nombre de la zona
lst- una lista del tipo zones
recibe también una lista con los nombres de las zonas cardiacas 

Busca recursivamente el nombre en la lista que recibió.

Si encuentra el nombre en la lista, devuelve el car de lst


Si no, hace una llamada recursiva con el mismo nombre y el el resto de las listas
|#

(define (get-zone-aux nombre lst lista) 
  (if (eqv? nombre (car lista));;comparamos el nombre con el primer elemento de lista
        (car lst);;con car, extraemos el elemento sin 'list'
  (get-zone-aux nombre (cdr lst) (cdr lista) )));;llamada recursiva con el resto de los nombres de ambas listas



(define my-zones (zones 50 180)) ;;definimos una lista de zonas (para las pruebas)

(test (get-zone 'resting my-zones) (resting 50 114.0))
(test (get-zone 'fat-burning my-zones) (fat-burning 128.0 140.0))
(test (get-zone 'aerobic my-zones) (aerobic 141.0 153.0))
(test (get-zone 'anaerobic my-zones) (anaerobic 154.0 166.0))
(test (get-zone 'maximum my-zones)(maximum 167.0 180))

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
