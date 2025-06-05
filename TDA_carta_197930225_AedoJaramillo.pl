%TDA carta

:-module(tda_carta_197930225_AedoJaramillo, [
    carta/5,
    cartaObtenerTipo/2,
    cartaObtenerDescripcion/2,
    cartaObtenerAccion/2
    ]).

%-----CONSTRUCTOR-----
% Descripcion: Constructor TDA carta
% Dominio: ID (integer) X Tipo (atom) X Descripcion (atom) X Carta (TDA Carta).
% Recorrido: carta
carta(ID, Tipo, Descripcion, Accion, Carta):-
    Carta = [ID, Tipo, Descripcion, Accion].

% Descripcion: Obtiene Tipo asociado a la carta
% Dominio: Carta (TDA carta) X Tipo (atom)
% Recorrido : atom
cartaObtenerTipo(Carta, Tipo):-
    Carta = [_, Tipo, _, _].

% Descripcion: Obtiene descripcion asociada a la carta
% Dominio: Carta (TDA carta) X Tipo (atom)
% Recorrido : atom
cartaObtenerDescripcion(Carta, Descripcion):-
    Carta = [_, _, Descripcion, _].

% Descripcion: Obtiene accion ligada a la carta
% Dominio: Carta (TDA Carta) X Accion (procedure).
% Recorrido: carta
cartaObtenerAccion(Carta, Accion):-
    Carta = [_, _, _, Accion].