:- use_module(library(lists)).

:- dynamic conexion/3.
:- dynamic tesoro/2.
:- dynamic camino_realizado/1.
:- dynamic jugador/1.
:- dynamic inventario/1.
:- dynamic objetos_usados/1.

donde_esta_objeto(Objeto) :- findall(Lugar, objeto(Objeto, Lugar), Lugares), 
    (   Lugares = []
    ->  write("El objeto "), write(Objeto), write(" no se encuentra en ningun lugar.")
    ;   write("El objeto "), write(Objeto), write(" se encuentra en: "),
        mostrar_lugares(Lugares)
    ).
    


que_tengo :-
    inventario(Inv),
    (   Inv = []
    ->  writeln("El inventario esta vacio")
    ;   writeln("Inventario:"),
        mostrar_objetos(Inv)
    ).