:- module(operadores_aux, [boolean/1, max/3, sumarLista/2, getDadoRandom/3]).

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

% Predicado myRandom
myRandom(Xn, Xn1):-
  Xn1 is ((1103515245 * Xn) + 12345) mod 2147483648.

% Predicado getDadoRandom que recibe la semilla y controla los resultados
getDadoRandom(Seed, NvaSeed, R):-
    myRandom(Seed, NvaSeed),
    R is 1 + (NvaSeed mod 6).