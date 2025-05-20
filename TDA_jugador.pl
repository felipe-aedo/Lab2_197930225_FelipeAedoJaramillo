% TDA jugador

:- module(tda_jugador, [ %Crear modulo a exportar
    jugador/8,
    es_jugador/1,
    jugador_obtener_id/2,
    jugador_obtener_nombre/2,
    jugador_obtener_dinero/2,
    jugador_obtener_propiedades/2,
    jugador_obtener_posicion/2,
    jugador_estaencarcel/2,
    jugador_obtener_CCarcel/2,
    jugador_set_id/3,
    jugador_set_nombre/3,
    jugador_set_dinero/3,
    jugador_agregar_propiedad/3,
    jugador_set_posicion/3,
    jugador_set_esta_en_carcel/3,
    jugador_set_cartas_carcel/3
    ]).

:- use_module(operadores_aux). %Importar libreria de operadores

%----CONSTRUCTOR----
jugador(ID, Nombre, Dinero, Propiedades, Posicion, EstaenCarcel, CartasCarcel, Player):-
    Player = [ID, Nombre, Dinero, Propiedades, Posicion, EstaenCarcel, CartasCarcel].


%----PERTENENCIA----
es_jugador(Jugador):-
    Jugador = [ID, Nombre, Dinero, Propiedades, Posicion, EstaenCarcel, CartasCarcel],
    integer(ID), atom(Nombre), integer(Dinero), is_list(Propiedades), forall(member(P, Propiedades), integer(P)),integer(Posicion), boolean(EstaenCarcel), integer(CartasCarcel),
    Posicion >= 0, Dinero >= 0, ID > 0, CartasCarcel >= 0.


%------GETTERS------
jugador_obtener_id(Jugador, ID):-
    Jugador = [ID, _, _, _, _, _, _].

jugador_obtener_nombre(Jugador, Nombre):-
    Jugador = [_, Nombre, _, _, _, _, _].

jugador_obtener_dinero(Jugador, Dinero):-
    Jugador = [_, _, Dinero, _, _, _, _].

jugador_obtener_propiedades(Jugador, Propiedades):-
    Jugador = [_, _, _, Propiedades, _, _, _].

jugador_obtener_posicion(Jugador, Posicion):-
    Jugador = [_, _, _, _, Posicion, _, _].

jugador_estaencarcel(Jugador, EstaenCarcel):-
    Jugador = [_, _, _, _, _, EstaenCarcel, _].

jugador_obtener_CCarcel(Jugador, CartasCarcel):-
    Jugador = [_, _, _, _, _, _, CartasCarcel].


%----SETTERS Y MODIFIERS----

jugador_set_id(Jugador0, NuevoID, JugadorActualizado):-
    Jugador0 = [_, Nombre, Dinero, Propiedades, Posicion, EstaenCarcel, CartasCarcel],
    JugadorActualizado = [NuevoID, Nombre, Dinero, Propiedades, Posicion, EstaenCarcel, CartasCarcel].

jugador_set_nombre(Jugador0, NuevoNombre, JugadorActualizado):-
    Jugador0 = [ID, _, Dinero, Propiedades, Posicion, EstaenCarcel, CartasCarcel],
    JugadorActualizado = [ID, NuevoNombre, Dinero, Propiedades, Posicion, EstaenCarcel, CartasCarcel].

jugador_set_dinero(Jugador0, NuevoDinero, JugadorActualizado):-
    Jugador0 = [ID, Nombre, _, Propiedades, Posicion, EstaenCarcel, CartasCarcel],
    JugadorActualizado = [ID, Nombre, NuevoDinero, Propiedades, Posicion, EstaenCarcel, CartasCarcel].

jugador_agregar_propiedad(Jugador0, NuevaPropiedad, JugadorActualizado):-
    Jugador0 = [ID, Nombre, Dinero, Propiedades, Posicion, EstaenCarcel, CartasCarcel],
    JugadorActualizado = [ID, Nombre, Dinero, [NuevaPropiedad|Propiedades], Posicion, EstaenCarcel, CartasCarcel].

jugador_set_posicion(Jugador0, NuevaPosicion, JugadorActualizado):-
    Jugador0 = [ID, Nombre, Dinero, Propiedades, _, EstaenCarcel, CartasCarcel],
    JugadorActualizado = [ID, Nombre, Dinero, Propiedades, NuevaPosicion, EstaenCarcel, CartasCarcel].

jugador_set_esta_en_carcel(Jugador0, EstadoCarcel, JugadorActualizado):-
    Jugador0 = [ID, Nombre, Dinero, Propiedades, Posicion, _, CartasCarcel],
    JugadorActualizado = [ID, Nombre, Dinero, Propiedades, Posicion, EstadoCarcel, CartasCarcel].

jugador_set_cartas_carcel(Jugador0, NuevasCartasCarcel, JugadorActualizado):-
    Jugador0 = [ID, Nombre, Dinero, Propiedades, Posicion, EstaenCarcel, _],
    JugadorActualizado = [ID, Nombre, Dinero, Propiedades, Posicion, EstaenCarcel, NuevasCartasCarcel].