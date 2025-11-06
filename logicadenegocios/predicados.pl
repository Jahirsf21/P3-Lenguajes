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

mostrar_lugares([]).
mostrar_lugares([Lugar|Resto]) :-
    write("- "), writeln(Lugar),
    mostrar_lugares(Resto).

mostrar_objetos([]).
mostrar_objetos([Objeto|Resto]) :-
    write("- "), writeln(Objeto),
    mostrar_objetos(Resto).

puedo_ir(LugarDestino) :-
    jugador(LugarActual),
    (   ruta_directa(LugarActual, LugarDestino)
    ->  objetos_requeridos(LugarDestino, ObjetosRequeridos),
        (   verificar_objetos_en_inventario(ObjetosRequeridos)
        ->  true
        ;   write("No puedes ir a "), write(LugarDestino), write("; te faltan los objetos requeridos."), fail
        )
    ;   write("No hay una conexion directa entre "), write(LugarActual), write(" y "), write(LugarDestino), fail
    ).

mover_hacia(LugarDestino) :-
    jugador(LugarActual),
    (   ruta_directa(LugarActual, LugarDestino)
    ->  objetos_requeridos(LugarDestino, ObjetosRequeridos),
        (   verificar_objetos_en_inventario_y_usados(ObjetosRequeridos)
        ->  retractall(jugador(_)),
            assertz(jugador(LugarDestino)),
            actualizar_camino_realizado(LugarDestino),
            write("Te has movido de "), write(LugarActual), write(" a "), write(LugarDestino), nl,
            verificar_tesoro(LugarDestino)
        ;   write("No puedes ir a "), write(LugarDestino), write("; te faltan los objetos requeridos o no los has usado."), nl,
            fail
        )
    ;   write("No hay una conexion directa entre "), write(LugarActual), write(" y "), write(LugarDestino), fail
    ).

objetos_requeridos(Lugar, Objetos) :-
    findall(Objeto, requiere(Objeto,Lugar), Objetos).

verificar_objetos_en_inventario([]) :- !.
verificar_objetos_en_inventario(Objetos) :- 
    inventario(Inv),
    forall(member(Objeto, Objetos), member(Objeto, Inv)).

verificar_objetos_en_inventario_y_usados([]) :- !.
verificar_objetos_en_inventario_y_usados(Objetos) :- 
    inventario(Inv),
    objetos_usados(Usados),
    forall(member(Objeto, Objetos), (member(Objeto, Inv), member(Objeto,Usados))).
