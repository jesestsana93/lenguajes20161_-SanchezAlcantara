# Calificaciones

## Practicas

### Practica 1

Hay que tener cuidado con los casos base en primes no checan y se cicla
la funcion si dan un 0 como parametro y en mconcat se da un error al ejecutar

> (mconcat '() '())
. . car: contract violation
  expected: pair?
  given: '()
  
lo cual deberia regresar simplemente '()

Tu implementacion de any? y every? hacen uso de if anidados cuando para eso existe cond, aparte tu implementacion de every? es erronea porque cuando tienes la lista vacia every? debe regresar #t

**Calificacion: 9**