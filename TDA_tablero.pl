%TDA tablero

:- module(tda_tablero, [
    tablero/5,
    tableroAgregarPropiedades/3,
    tableroAgregarCasillasEspeciales/3,
    tableroObtenerUltimaPosicion/2
    ]).

:- use_module([operadores_aux, tda_propiedad]).

%-----CONSTRUCTOR-----
% Descripcion: Constructor TDA tablero
% Dominio: Propiedades (lista) X CartasSuerte (lista) X CartasComunidad (lista) X CasillasEspeciales (lista) X Tablero (board)
% Recorrido: tablero
tablero(Propiedades, CartasSuerte, CartasComunidad, CasillasEspeciales, Tablero):-
    Tablero = [Propiedades, CartasSuerte, CartasComunidad, CasillasEspeciales].

%-----GETTERS-----
% Descripcion: Obtiene las propiedades con su posicion (lista de pares)
% Dominio:
% Recorrido:
tableroGetPropiedades(Tablero, Propiedades):-
    Tablero = [Propiedades, _, _, _].

% Descripcion: Obtiene las casillas especiales con su posicion (lista de pares)
% Dominio:
% Recorrido:
tableroGetCasillas(Tablero, Casillas):-
    Tablero = [_, _, _, Casillas].

% Descripcion: Obtiene la ultima posicion en el tablero.
% Dominio:
% Recorrido:
tableroObtenerUltimaPosicion(Tablero, Posicion):-
    tableroGetPropiedades(Tablero, Propiedades), tableroGetCasillas(Tablero, Casillas),
    ultimaPosicion(Propiedades, Casillas, Posicion).


% Descripcion: Encuentra la ultima posicion en el tablero
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


%-----SETTERS Y MODIFIERS-----
% Descripcion: Actualiza lista de propiedades en el tablero.
% Dominio: TableroIN (TDA tablero) X Propiedades (Lista de pares) X TableroOUT (TDA tablero).
% Recorrido: tablero
tableroAgregarPropiedades(TableroIN, Propiedades, TableroOUT):-
    TableroIN = [_, CartasSuerte, CartasComunidad, Casillas],
    TableroOUT= [Propiedades, CartasSuerte, CartasComunidad, Casillas].

% Descripcion: Actualiza lista de casillas especiales en el tablero.
% Dominio: TableroIN (TDA tablero) X Casillas con posicion (Lista de pares) X TableroOUT (TDA tablero).
% Recorrido: tablero
tableroAgregarCasillasEspeciales(TableroIN, Casillas, TableroOUT):-
    TableroIN = [Propiedades, CartasSuerte, CartasComunidad, _],
    TableroOUT= [Propiedades, CartasSuerte, CartasComunidad, Casillas].