:- use_module(library(lists)).

:- dynamic conexion/3.
:- dynamic tesoro/2.
:- dynamic camino_realizado/1.
:- dynamic jugador/1.
:- dynamic inventario/1.
:- dynamic objetos_usados/1.

camino_realizado([]).
jugador(bosque).
inventario([]).
objetos_usados([]).



lugar(bosque, "Bosque sombrío con árboles milenarios."). 
lugar(puente, "Un viejo puente de madera."). 
lugar(cueva, "Cueva donde habita el dragón."). 
lugar(templo, "Templo abandonado con inscripciones antiguas."). 
conectado(bosque, puente). 
conectado(puente, cueva). 
conectado(bosque, templo). 
objeto(llave, templo). 
objeto(espada, bosque). 
objeto(escudo, puente). 
requiere(llave, cueva). 
requiere(espada, puente). 
requiereVisita(cueva, bosque). 
tesoro(templo, escudo).