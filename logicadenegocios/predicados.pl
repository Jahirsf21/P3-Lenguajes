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
    ruta_directa(LugarActual, LugarDestino),
    objetos_requeridos(LugarDestino, ObjetosRequeridos),
    verificar_objetos_en_inventario(ObjetosRequeridos).

mover(LugarDestino) :-
    jugador(LugarActual),
    ruta_directa(LugarActual, LugarDestino),
    objetos_requeridos(LugarDestino, ObjetosRequeridos),
    verificar_objetos_en_inventario_y_usados(ObjetosRequeridos),
    retractall(jugador(_)),
    write("UI2 "),
    assertz(jugador(LugarDestino)),
    actualizar_camino_realizado(LugarDestino).


tomar(Objeto) :- jugador(Lugar), objeto(Objeto, Lugar), inventario(Inv), \+ member(Objeto, Inv), retract(inventario(Inv)), assertz(inventario([Objeto|Inv])).

usar(Objeto) :- inventario(Inv), member(Objeto, Inv), objetos_usados(Usados), \+ member(Objeto, Usados), retract(objetos_usados(Usados)), assertz(objetos_usados([Objeto|Usados])).

que_tengo(X) :-
    inventario(X).

donde_esta(Objeto, X) :-
    findall(Lugar, objeto(Objeto, Lugar), Lugares),
    (   Lugares = []
    ->  X = []
    ;   X = Lugares
    ).


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

lugar_visitados(X) :-
    camino_realizado(X).

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

como_gano(X) :-
    jugador(Inicio),
    findall(
        ruta(LugarFinal, Objeto, Camino),
        ruta(Inicio, LugarFinal, Objeto, Camino),
        Rutas
    ),
    (   Rutas = []
    ->  X = []
    ;   convertir_rutas(Rutas, X)
    ).

convertir_rutas([], []).
convertir_rutas([ruta(LugarFinal, Objeto, Camino)|Resto], 
                [[LugarFinal, Objeto, Camino]|Xs]) :-
    convertir_rutas(Resto, Xs).





mostrar_rutas_gane([]).
mostrar_rutas_gane([ruta(LugarFinal, Objeto, Camino)|Resto]) :-
    write('Destino: '), writeln(LugarFinal),
    write('Objeto requerido: '), writeln(Objeto),
    writeln('Camino sugerido:'),
    mostrar_lugares(Camino),
    writeln(''),
    mostrar_rutas_gane(Resto).

verifica_gane(X) :-
    jugador(LugarActual),
    inventario(Inv),
    findall(Objeto, (tesoro(LugarActual, Objeto), member(Objeto, Inv)), Condiciones),
    camino_realizado(Camino),
    (   Condiciones = []
    ->  X = []
    ;   X = [Camino, Inv, Condiciones]
    ).
