:- use_module(library(lists)).

:- dynamic conexion/3.
:- dynamic tesoro/2.
:- dynamic camino_realizado/1.
:- dynamic jugador/1.
:- dynamic inventario/1.
:- dynamic objetos_usados/1.

% busca una ruta directa entre el origen y el destino dados como parametros.
ruta_directa(Origen, Destino) :-
    (   conectado(Origen, Destino)
    ;   conectado(Destino, Origen)
    ;   conexion(Origen, Destino, _)
    ;   conexion(Destino, Origen, _)
    ).

% devuelve todos los objetos requeridos para poder ir a Lugar, en la variable Objetos
% si el lugar no existe devuelve una lista vacia
objetos_requeridos(Lugar, Objetos) :-
    findall(Objeto, requiere(Objeto, Lugar), Objetos).

% verifica si todos los objetos requeridos se encuentran en el inventario
% si la entrada es vacia, hace cut para que evite el backtracking
verificar_objetos_en_inventario([]) :- !.
verificar_objetos_en_inventario(Objetos) :-
    inventario(Inv),
    forall(member(Objeto, Objetos), member(Objeto, Inv)).

% verifica si todos los objetos requeridos están en el inventario y ya fueron usados
% si la entrada es vacia, hace cut para que evite el backtracking
verificar_objetos_en_inventario_y_usados([]) :- !.
verificar_objetos_en_inventario_y_usados(Objetos) :-
    inventario(Inv),
    objetos_usados(Usados),
    forall(member(Objeto, Objetos), (member(Objeto, Inv), member(Objeto, Usados))).

% agrega un Lugar al camino recorrido por el jugador
actualizar_camino_realizado(Lugar) :-
    camino_realizado(CaminoActual),
    append(CaminoActual, [Lugar], CaminoActualizado),
    retractall(camino_realizado(_)),
    assertz(camino_realizado(CaminoActualizado)).

% determina si el jugador puede ir a un destino considerando las conexiones y objetos requeridos
puedo_ir(LugarDestino) :-
    jugador(LugarActual),
    ruta_directa(LugarActual, LugarDestino),
    objetos_requeridos(LugarDestino, ObjetosRequeridos),
    verificar_objetos_en_inventario(ObjetosRequeridos).

% mueve al jugador a un nuevo lugar si cumple con las condiciones necesarias
mover(LugarDestino) :-
    jugador(LugarActual),
    ruta_directa(LugarActual, LugarDestino),
    objetos_requeridos(LugarDestino, ObjetosRequeridos),
    verificar_objetos_en_inventario_y_usados(ObjetosRequeridos),
    retractall(jugador(_)),
    assertz(jugador(LugarDestino)),
    actualizar_camino_realizado(LugarDestino).

% permite al jugador tomar un objeto si está en el mismo lugar y no lo tiene en su inventario
tomar(Objeto) :-
    jugador(Lugar),
    objeto(Objeto, Lugar),
    inventario(Inv),
    \+ member(Objeto, Inv),
    retract(inventario(Inv)),
    assertz(inventario([Objeto|Inv])).

% marca un objeto del inventario como usado
usar(Objeto) :-
    inventario(Inv),
    member(Objeto, Inv),
    objetos_usados(Usados),
    \+ member(Objeto, Usados),
    retract(objetos_usados(Usados)),
    assertz(objetos_usados([Objeto|Usados])).

% muestra los objetos que el jugador tiene actualmente
que_tengo(X) :-
    inventario(X).

% devuelve los lugares donde se encuentra un objeto dado
donde_esta(Objeto, X) :-
    findall(Lugar, objeto(Objeto, Lugar), Lugares),
    (   Lugares = []
    ->  X = []
    ;   X = Lugares
    ).


% muestra los lugares a los que se puede ir desde la ubicación actual
lugares_conectados :-
    jugador(LugarActual),
    write("Desde "), write(LugarActual), writeln(" puedes ir a:"),
    findall(Destino, (ruta_directa(LugarActual, Destino), Destino \= LugarActual), Destinos),
    mostrar_lugares(Destinos).

% devuelve los lugares que el jugador ha visitado
lugar_visitados(X) :-
    camino_realizado(X).

% encuentra una ruta entre dos lugares (Inicio y Fin) y devuelve el camino recorrido
ruta(Inicio, Fin, Camino) :-
    ruta_buscar(Inicio, Fin, [Inicio], CaminoInvertido),
    reverse(CaminoInvertido, Camino).

% caso base del predicado: cuando se llega al destino final
ruta_buscar(Fin, Fin, Visitados, Visitados).

% busca recursivamente rutas entre dos lugares
ruta_buscar(Actual, Fin, Visitados, Camino) :-
    ruta_directa(Actual, Siguiente),
    \+ member(Siguiente, Visitados),
    ruta_buscar(Siguiente, Fin, [Siguiente|Visitados], Camino).

% verifica si todos los movimientos en una ruta son válidos según los objetos requeridos
movimientos_validos([_]).
movimientos_validos([Actual, Siguiente|Resto]) :-
    ruta_directa(Actual, Siguiente),
    objetos_requeridos(Siguiente, Objetos),
    verificar_objetos_en_inventario(Objetos),
    movimientos_validos([Siguiente|Resto]).

% busca una ruta que lleve a un lugar con un tesoro, teniendo el objeto correspondiente
ruta(Inicio, LugarFinal, Objeto, Camino) :-
    inventario(Inv),
    tesoro(LugarFinal, Objeto),
    member(Objeto, Inv),
    ruta(Inicio, LugarFinal, Camino),
    movimientos_validos(Camino).

% genera todas las rutas posibles para ganar (encontrar tesoros con los objetos correctos)
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

% convierte la estructura interna de rutas en una lista mas legible
convertir_rutas([], []).
convertir_rutas([ruta(LugarFinal, Objeto, Camino)|Resto], 
                [[LugarFinal, Objeto, Camino]|Xs]) :-
    convertir_rutas(Resto, Xs).


% verifica si el jugador ha ganado (tiene el tesoro requerido en el lugar correcto)
verifica_gane(X) :-
    jugador(LugarActual),
    inventario(Inv),
    findall(Objeto, (tesoro(LugarActual, Objeto), member(Objeto, Inv)), Condiciones),
    camino_realizado(Camino),
    (   Condiciones = []
    ->  X = []
    ;   X = [Camino, Inv, Condiciones]
    ).
