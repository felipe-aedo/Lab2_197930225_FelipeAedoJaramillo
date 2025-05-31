:- module(operadores_aux_197930225_AedoJaramillo, [boolean/1, max/3, sumarLista/2, getDadoRandom/3, myMember/2]).

%Operadores de pertenencia booleana
boolean(true).
boolean(false).

%obtiene el maximo entre dos numeros
% dominio: X (number) , Y (number) , Maximo entre ambos(number)
max(X, Y, X) :- X >= Y.
max(X, Y, Y) :- Y > X.


%comprueba si el elemento pertenece a la lista
% dominio: elemento, lista de elementos
myMember(X,[X|_]):- !.
myMember(X,[_|Y]):- myMember(X, Y).

%Suma los valores contenidos en una lista
% Caso base.
sumarLista([], 0).
% Caso recursivo.
sumarLista([Head|Tail], Suma) :-
    sumarLista(Tail, SumaCola),
    Suma is Head + SumaCola.

% Predicado myRandom
%dominio: Xn (integer), Xn1 (integer)
myRandom(Xn, Xn1):-
  Xn1 is ((1103515245 * Xn) + 12345) mod 2147483648.

% Predicado getDadoRandom que recibe la semilla y controla los resultados
%dominio: Seed(integer) , NvaSeed (integer), R (integer)
getDadoRandom(Seed, NvaSeed, R):-
    myRandom(Seed, NvaSeed),
    R is 1 + (NvaSeed mod 6).