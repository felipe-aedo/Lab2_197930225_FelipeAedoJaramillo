:- module(operadores_aux, [boolean/1, max/3]).

boolean(true).
boolean(false).

max(X, Y, X) :- X >= Y.
max(X, Y, Y) :- Y > X.