%TDA carta

:-module(tda_carta, [
    carta/5
    ]).

%-----CONSTRUCTOR-----
% Descripcion: Constructor TDA carta
% Dominio: ID (integer) X Tipo (atom) X Descripcion (atom) X Carta (TDA Carta).
% Recorrido: carta
carta(ID, Tipo, Descripcion, Accion, Carta):-
    Carta = [ID, Tipo, Descripcion, Accion].