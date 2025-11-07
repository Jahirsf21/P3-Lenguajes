:- use_module(library(lists)).

:- dynamic conexion/3.
:- dynamic tesoro/2.
:- dynamic camino_realizado/1.
:- dynamic jugador/1.
:- dynamic inventario/1.
:- dynamic objetos_usados/1.

ruta_directa(Origen, Destino) :-
    (   conectado(Origen, Destino)
    ;   conectado(Destino, Origen)
    ;   conexion(Origen, Destino, _)
    ;   conexion(Destino, Origen, _)
    ).

objetos_requeridos(Lugar, Objetos) :-
    findall(Objeto, requiere(Objeto, Lugar), Objetos).

verificar_objetos_en_inventario([]) :- !.
verificar_objetos_en_inventario(Objetos) :-
    inventario(Inv),
    forall(member(Objeto, Objetos), member(Objeto, Inv)).

verificar_objetos_en_inventario_y_usados([]) :- !.
verificar_objetos_en_inventario_y_usados(Objetos) :-
    inventario(Inv),
    objetos_usados(Usados),
    forall(member(Objeto, Objetos), (member(Objeto, Inv), member(Objeto, Usados))).

actualizar_camino_realizado(Lugar) :-
    camino_realizado(CaminoActual),
    append(CaminoActual, [Lugar], CaminoActualizado),
    retractall(camino_realizado(_)),
    assertz(camino_realizado(CaminoActualizado)).

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

tomar_objeto(Objeto) :- jugador(Lugar), objeto(Objeto, Lugar), inventario(Inv), \+ member(Objeto, Inv), retract(inventario(Inv)), assertz(inventario([Objeto|Inv])).

usar_objeto(Objeto) :- inventario(Inv), member(Objeto, Inv), objetos_usados(Usados), \+ member(Objeto, Usados), retract(objetos_usados(Usados)), assertz(objetos_usados([Objeto|Usados])).

que_tengo :-
    inventario(Inv),
    (   Inv = []
    ->  writeln("El inventario esta vacio")
    ;   writeln("Inventario:"),
        mostrar_objetos(Inv)
    ).

mostrar_objetos([]).
mostrar_objetos([Objeto|Resto]) :-
    write("- "), writeln(Objeto),
    mostrar_objetos(Resto).

donde_esta_objeto(Objeto) :- findall(Lugar, objeto(Objeto, Lugar), Lugares),
    (   Lugares = []
    ->  write("El objeto "), write(Objeto), write(" no se encuentra en ningun lugar.")
    ;   write("El objeto "), write(Objeto), write(" se encuentra en: "),
        mostrar_lugares(Lugares)
    ).

mostrar_lugares([]).
mostrar_lugares([Lugar|Resto]) :-
    write("- "), writeln(Lugar),
    mostrar_lugares(Resto).

donde_estoy :-
    jugador(Lugar),
    lugar(Lugar, Descripcion),
    write("Estas en: "), writeln(Lugar),
    write("Descripcion: "), writeln(Descripcion),
    write("Objetos aqui: "),
    findall(Obj, objeto(Obj, Lugar), Objetos),
    (   Objetos = []
    ->  writeln("ninguno")
    ;   mostrar_objetos(Objetos)
    ).

lugares_conectados :-
    jugador(LugarActual),
    write("Desde "), write(LugarActual), writeln(" puedes ir a:"),
    findall(Destino, (ruta_directa(LugarActual, Destino), Destino \= LugarActual), Destinos),
    mostrar_lugares(Destinos).

lugar_visitados :-
    camino_realizado(Camino),
    (   Camino = []
    ->  writeln("Aun no has visitado ningún lugar.")
    ;   writeln("Lugares visitados:"),
        mostrar_lugares(Camino)
    ).

ruta(Inicio, Fin, Camino) :-
    ruta_buscar(Inicio, Fin, [Inicio], CaminoInvertido),
    reverse(CaminoInvertido, Camino).

ruta_buscar(Fin, Fin, Visitados, Visitados).
ruta_buscar(Actual, Fin, Visitados, Camino) :-
    ruta_directa(Actual, Siguiente),
    \+ member(Siguiente, Visitados),
    ruta_buscar(Siguiente, Fin, [Siguiente|Visitados], Camino).

movimientos_validos([_]).
movimientos_validos([Actual, Siguiente|Resto]) :-
    ruta_directa(Actual, Siguiente),
    objetos_requeridos(Siguiente, Objetos),
    verificar_objetos_en_inventario(Objetos),
    movimientos_validos([Siguiente|Resto]).

ruta(Inicio, LugarFinal, Objeto, Camino) :-
    inventario(Inv),
    tesoro(LugarFinal, Objeto),
    member(Objeto, Inv),
    ruta(Inicio, LugarFinal, Camino),
    movimientos_validos(Camino).

como_gano :-
    jugador(Inicio),
    findall(ruta(LugarFinal, Objeto, Camino),
            ruta(Inicio, LugarFinal, Objeto, Camino),
            Rutas),
    (   Rutas = []
    ->  writeln("No existen rutas de gane con el inventario actual."),
        fail
    ;   writeln("Rutas posibles para ganar:"),
        mostrar_rutas_gane(Rutas)
    ).


