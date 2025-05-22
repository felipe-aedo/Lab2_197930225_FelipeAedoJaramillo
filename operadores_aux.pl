:- module(operadores_aux, [boolean/1, max/3, sumarLista/2]).

boolean(true).
boolean(false).

max(X, Y, X) :- X >= Y.
max(X, Y, Y) :- Y > X.

% Caso base.
sumarLista([], 0).

% Caso recursivo.
sumarLista([Head|Tail], Suma) :-
    sumarLista(Tail, SumaCola),
    Suma is Head + SumaCola.