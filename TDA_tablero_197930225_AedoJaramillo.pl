%TDA tablero

:- module(tda_tablero_197930225_AedoJaramillo, [
    tablero/5,
    tableroAgregarPropiedades/3,
    tableroAgregarCasillasEspeciales/3,
    tableroAgregarCartas/4,
    tableroObtenerPosicion/3,
    tableroObtenerPropiedad/3,
    tableroObtenerCartas/3,
    tableroObtenerUltimaPosicion/2,
    tableroObtenerCarcel/2,
    tableroActualizarPropiedad/3
    ]).

:- use_module([operadores_aux_197930225_AedoJaramillo, tda_propiedad_197930225_AedoJaramillo]).

%-----CONSTRUCTOR-----
% Descripcion: Constructor TDA tablero
% Dominio: Propiedades (lista) X CartasSuerte (lista) X CartasComunidad (lista) X CasillasEspeciales (lista) X Tablero (board)
% Recorrido: tablero
tablero(Propiedades, CartasSuerte, CartasComunidad, CasillasEspeciales, Tablero):-
    Tablero = [Propiedades, CartasSuerte, CartasComunidad, CasillasEspeciales].

%-----GETTERS-----
% Descripcion: Obtiene las propiedades con su posicion (lista de pares)
% Dominio: Tablero (TDA tablero) X Propiedades (lista de pares)
% Recorrido: lista de pares
tableroObtenerPropiedades(Tablero, Propiedades):-
    Tablero = [Propiedades, _, _, _].

% Descripcion: Obtiene la propiedad de id especifico
% Dominio: Tablero (TDA tablero) X Id (integer) X Propiedad (TDA propiedad)
% Recorrido: propiedad
tableroObtenerPropiedad(Tablero, Id, Propiedad):-
    tableroObtenerPropiedades(Tablero, Propiedades),
    encontrarPropiedad(Propiedades, Id, Propiedad).
% predicado auxiliar 
% Recursion de cola
encontrarPropiedad([[Propiedad,_]|_], Id, Propiedad) :- 
    propiedadObtenerId(Propiedad, Id2), Id = Id2, !.
encontrarPropiedad([_|RestoProps], Id, Propiedad):-
    encontrarPropiedad(RestoProps, Id, Propiedad).

% Descripcion: Obtiene las cartas de un tipo especificado
% Dominio: Tablero (TDA tablero) X TipoCarta (Atom) X Cartas (Lista de cartas)
% Recorrido: lista de cartas
tableroObtenerCartas(Tablero, TipoCarta, Cartas):-
    Tablero = [_, Cartas, _, _],
    TipoCarta = suerte.
tableroObtenerCartas(Tablero, TipoCarta, Cartas):-
    Tablero = [_, _, Cartas, _],
    TipoCarta = comunidad.

% Descripcion: Obtiene las casillas especiales con su posicion (lista de pares)
% Dominio: Tablero (TDA tablero) X Casillas (lista de pares)
% Recorrido: lista de pares
tableroObtenerCasillas(Tablero, Casillas):-
    Tablero = [_, _, _, Casillas].

% Descripcion: Obtiene el elemento en la posicion especificada (propiedad o casilla especial)
% Dominio : Tablero (TDA tablero) X Posicion (integer) X Elemento (Propiedad o casillaEspecial)
% Recorrido : casilla o propiedad
tableroObtenerPosicion(Tablero, Posicion, Elemento):-
    tableroObtenerPropiedades(Tablero, Props),
    tableroObtenerCasillas(Tablero, Casillas),
    (   buscarPorPosicion(Props, Posicion, Elemento)
    ;   buscarPorPosicion(Casillas, Posicion, Elemento)
    ), !.

%Predicado auxiliar para encontrar posicion en el tablero
% Dominio: lista de pares(propiedad, posicion) X posicion (integer) X elemento (TDA propiedad o casilla)
% Recorrido: propiedad o casilla
%Recursion Natural
buscarPorPosicion([[Elem, Pos]|_], Pos, Elem):- !.
buscarPorPosicion([_|Resto], Pos, Elem):-
    buscarPorPosicion(Resto, Pos, Elem).

% Descripcion: Obtiene la ultima posicion en el tablero.
% Dominio: Tablero (TDA tablero) X Posicion (integer)
% Recorrido: integer
tableroObtenerUltimaPosicion(Tablero, Posicion):-
    tableroObtenerPropiedades(Tablero, Propiedades), tableroObtenerCasillas(Tablero, Casillas),
    ultimaPosicion(Propiedades, Casillas, Posicion).

% Descripcion: Encuentra la ultima posicion en un par de listas de pares
% Dominio: Propiedades (lista de pares) X Casillas (lista de pares) X UltimaPosicion (integer)
% Recorrido: integer
% Recursion de cola
ultimaPosicion(Propiedades, Casillas, UltimaPosicion):-
    ultimaPosicionAux(Propiedades, Casillas, 0, UltimaPosicion).
% Descripcion: Predicado auxiliar para encontrar la ultima posicion en el tablero (dividido en 4 casos)
ultimaPosicionAux([], [], UltimaPosicion, UltimaPosicion):- !. %Caso base: No queda nada por comparar.
% Casos de comparacion :
ultimaPosicionAux([], [[_,PosCasilla]| ColaCasillas], Actual, NuevoMaximo) :-  %Caso : No quedan propiedades
    max(PosCasilla, Actual, IntermedioMax), ultimaPosicionAux([], ColaCasillas, IntermedioMax, NuevoMaximo).
ultimaPosicionAux([[_,PosPropiedad]| ColaPropiedades], [], Actual, NuevoMaximo) :- %Caso : No quedan casillas
    max(PosPropiedad, Actual, IntermedioMax), ultimaPosicionAux(ColaPropiedades, [], IntermedioMax, NuevoMaximo).
ultimaPosicionAux([[_, PosPropiedad]| ColaPropiedades], [[_, PosCasilla]| ColaCasillas], Actual, NuevoMaximo):- %Caso : Quedan ambas por comparar
    max(PosPropiedad, PosCasilla, M1), max(M1, Actual, IntermedioMax), ultimaPosicionAux(ColaPropiedades, ColaCasillas, IntermedioMax, NuevoMaximo).


% Descripcion: Obtiene la posicion de la carcel en el tablero.
% Dominio: Tablero (TDA tablero) X Posicion (integer)
% Recorrido: integer
tableroObtenerCarcel(Tablero, Posicion):-
    tableroObtenerCasillas(Tablero, Casillas),
    encontrarCarcel(Casillas, Posicion).
% Predicado auxiliar para encontrar la posicion de la carcel
% Dominio: Lista de casillas con posicion (lista de pares) X Posicion (integer)
% Recorrido: integer
% Recursion natural
encontrarCarcel([[carcel, Posicion]|_], Posicion):- !.
encontrarCarcel([_|RestoCasillas], Posicion):- 
    encontrarCarcel(RestoCasillas, Posicion).

%-----SETTERS Y MODIFIERS-----
% Descripcion: Actualiza lista de propiedades en el tablero.
% Dominio: Tablero (TDA tablero) X Propiedades (Lista de pares) X TableroActualizado (TDA tablero).
% Recorrido: tablero
tableroAgregarPropiedades(Tablero, Propiedades, TableroActualizado):-
    Tablero = [_, CartasSuerte, CartasComunidad, Casillas],
    TableroActualizado = [Propiedades, CartasSuerte, CartasComunidad, Casillas].

% Descripcion: Actualiza lista de casillas especiales en el tablero.
% Dominio: Tablero (TDA tablero) X Casillas con posicion (Lista de pares) X TableroActualizado (TDA tablero).
% Recorrido: tablero
tableroAgregarCasillasEspeciales(Tablero, Casillas, TableroActualizado):-
    Tablero = [Propiedades, CartasSuerte, CartasComunidad, _],
    TableroActualizado = [Propiedades, CartasSuerte, CartasComunidad, Casillas].

% Descripcion: Agrega las cartas de suerte y de comunidad al tablero
% Dominio: Tablero (TDA tablero) X CartasSuerte (lista de cartas) X CartasComunidad (lista de cartas) X TableroActualizado (TDA tablero).
% Recorrido: tablero
tableroAgregarCartas(Tablero, CartasSuerte, CartasComunidad, TableroActualizado):-
    Tablero = [Propiedades, _, _, Casillas],
    TableroActualizado = [Propiedades, CartasSuerte, CartasComunidad, Casillas].

% Descripcion: Actualiza una propiedad en el tablero
% Dom: Tablero (TDA tablero) X Propiedad (TDA propiedad) X TableroActualizado (TDA tablero)
% Rec: tablero
tableroActualizarPropiedad(Tablero, Propiedad, TableroActualizado):-
    tableroObtenerPropiedades(Tablero, Propiedades),
    actualizarPropiedad(Propiedades, Propiedad, PropiedadesActualizadas),
    tableroAgregarPropiedades(Tablero, PropiedadesActualizadas, TableroActualizado).

% Descripcion: Actualiza una propiedad en una lista de propiedades
% Dominio: lista de propiedades X Propiedad X lista actualizada
% Recorrido: lista de propiedades
% Recursion natural
actualizarPropiedad([], _, []).
actualizarPropiedad([[PropActual, Posicion] | Resto], PropNueva, [[PropNueva, Posicion]| Resto]) :-
    propiedadObtenerId(PropActual, Id1),
    propiedadObtenerId(PropNueva, Id2),
    Id1 =:= Id2, !.
actualizarPropiedad([[PropActual, Posicion] | Resto], PropNueva, [[PropActual, Posicion] | RestoActualizado]) :-
    actualizarPropiedad(Resto, PropNueva, RestoActualizado).