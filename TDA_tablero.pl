%TDA tablero

:- module(tda_tablero, [
    tablero/5,
    tableroAgregarPropiedades/3
    ]).

%-----CONSTRUCTOR-----
% Descripcion: Constructor TDA tablero
% Dominio: Propiedades (lista) X CartasSuerte (lista) X CartasComunidad (lista) X CasillasEspeciales (lista) X Tablero (board)
% Recorrido: tablero
tablero(Propiedades, CartasSuerte, CartasComunidad, CasillasEspeciales, Tablero):-
    Tablero = [Propiedades, CartasSuerte, CartasComunidad, CasillasEspeciales].

%-----SETTERS Y MODIFIERS-----
% Descripcion: Actualiza lista de propiedades en el tablero.
% Dominio: TableroIN (TDA tablero) X Propiedades (Lista de pares) X TableroOUT (TDA tablero).
% Recorrido: tablero
tableroAgregarPropiedades(TableroIN, Propiedades, TableroOUT):-
    TableroIN = [_, CartasSuerte, CartasComunidad, CasillasEspeciales],
    TableroOUT= [Propiedades, CartasSuerte, CartasComunidad, CasillasEspeciales].