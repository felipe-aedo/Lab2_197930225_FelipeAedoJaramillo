%TDA propiedad

:- module(tda_propiedad_197930225_AedoJaramillo, [
    propiedad/9,
    esPropiedad/1,
    propiedadObtenerId/2,
    propiedadObtenerPrecio/2,
    propiedadObtenerRenta/2,
    propiedadObtenerDueno/2,
    propiedadObtenerCasas/2,
    propiedadEsHotel/1,
    propiedadEstaHipotecada/1,
    propiedadSetDueno/3,
    propiedadSetCasas/3,
    propiedadSetHotel/3,
    propiedadHipotecar/2,
    propiedadDeshipotecar/2
    ]).

:- use_module(operadores_aux_197930225_AedoJaramillo).

%-----CONSTRUCTOR-----
% Descripcion: Constructor TDA propiedad
% Dominio: Dominio: ID (integer) X Nombre (atom) X Precio (integer) X Renta (integer) X Dueno (integer) X Casas (integer) X EsHotel (boolean) X EstaHipotecada (boolean)
%          X Propiedad (TDA propiedad)
% Recorrido: propiedad
propiedad(ID, Nombre, Precio, Renta, Dueno, Casas, EsHotel, EstaHipotecada, Propiedad):-
    Propiedad = [ID, Nombre, Precio, Renta, Dueno, Casas, EsHotel, EstaHipotecada].

%----VERIFICADOR----
% Descripcion: Comprueba si pertenece al TDA propiedad
% Dominio: ID (integer) X Nombre (atom) X Precio (integer) X Renta (integer) X Dueno (integer) X Casas (integer) X EsHotel (boolean) X EstaHipotecada (boolean)
% Recorrido: boolean
esPropiedad(Propiedad):-
    Propiedad = [ID, Nombre, Precio, Renta, Dueno, Casas, EsHotel, EstaHipotecada],
    integer(ID), atom(Nombre), integer(Precio), integer(Renta), integer(Casas), boolean(EsHotel), boolean(EstaHipotecada),
    Precio > 0, Renta > 0, ID > 0, Casas >= 0, (Dueno = [] ; (integer(Dueno))).

%------GETTERS------
% Descripcion: Obtiene ID de la propiedad
% Dominio: Propiedad (TDA propiedad) X ID (integer)
% Recorrido: integer
propiedadObtenerId(Propiedad, ID):-
    Propiedad = [ID,_,_,_,_,_,_,_].

% Descripcion: Obtiene precio de la propiedad
% Dominio: Propiedad (TDA propiedad) X Precio (integer)
% Recorrido: integer
propiedadObtenerPrecio(Propiedad, Precio):-
    Propiedad = [_,_,Precio,_,_,_,_,_].

% Descripcion: Obtiene renta de la propiedad
% Dominio: Propiedad (TDA propiedad) X Renta (integer)
% Recorrido: integer
propiedadObtenerRenta(Propiedad, Renta):-
    Propiedad = [_,_,_,Renta,_,_,_,_].

% Descripcion: Obtiene dueno de la propiedad
% Dominio: Propiedad (TDA propiedad) X Dueno (integer)
% Recorrido: integer
propiedadObtenerDueno(Propiedad, Dueno):-
    Propiedad = [_,_,_,_,Dueno,_,_,_].

% Descripcion: Obtiene cantidad de casas en la propiedad
% Dominio: Propiedad (TDA propiedad) X Casas (integer)
% Recorrido: integer
propiedadObtenerCasas(Propiedad, Casas):-
    Propiedad = [_,_,_,_,_,Casas,_,_].

% Descripcion: Comprueba si la propiedad es hotel
% Dominio: Propiedad (TDA propiedad)
% Recorrido: boolean
propiedadEsHotel(Propiedad):-
    Propiedad = [_,_,_,_,_,_,EsHotel,_], EsHotel = true.

% Descripcion: Comprueba si esta hipotecada
% Dominio: Propiedad (TDA propiedad)
% Recorrido: boolean
propiedadEstaHipotecada(Propiedad):-
    Propiedad = [_,_,_,_,_,_,_,Estado], Estado = true.

%------SETTERS------
% Descripcion: Actualiza el dueño de una propiedad
% Dominio: Propiedad (TDA propiedad) X Dueno (integer) X PropiedadActualizada (TDA propiedad)
% Recorrido: propiedad
propiedadSetDueno(Propiedad, Dueno, PropiedadActualizada):-
    Propiedad = [ID, Nombre, Precio, Renta, _, Casas, EsHotel, EstaHipotecada],
    propiedad(ID, Nombre, Precio, Renta, Dueno, Casas, EsHotel, EstaHipotecada, PropiedadActualizada).

% Descripcion: Actualiza cantidad de casas en una propiedad
% Dominio: Propiedad (TDA propiedad) X Casas (integer) X PropiedadActualizada (TDA propiedad)
% Recorrido: propiedad
propiedadSetCasas(Propiedad, Casas, PropiedadActualizada):-
    Propiedad = [ID, Nombre, Precio, Renta, Dueno, _, EsHotel, EstaHipotecada],
    propiedad(ID, Nombre, Precio, Renta, Dueno, Casas, EsHotel, EstaHipotecada, PropiedadActualizada).

% Descripcion: Cambia el estado de hotel de la propiedad
% Dominio: Propiedad (TDA propiedad) X Estado (boolean) X PropiedadActualizada (TDA propiedad)
% Recorrido: propiedad
propiedadSetHotel(Propiedad, Estado, PropiedadActualizada):-
    Propiedad = [ID, Nombre, Precio, Renta, Dueno, Casas, _, EstaHipotecada],
    propiedad(ID, Nombre, Precio, Renta, Dueno, Casas, Estado, EstaHipotecada, PropiedadActualizada).

% Descripcion: Cambia el estado a hipotecada
% Dominio: PropiedadIN (TDA propiedad) X PropiedadOut (TDA propiedad)
% Recorrido: propiedad
propiedadHipotecar(Propiedad, PropiedadActualizada):-
    Propiedad = [ID, Nombre, Precio, Renta, Dueno, Casas, EsHotel, EstadoHipoteca], EstadoHipoteca \= true,
    propiedad(ID, Nombre, Precio, Renta, Dueno, Casas, EsHotel, true, PropiedadActualizada).

% Descripcion: Levanta la hipoteca de una propiedad
% Dominio: PropiedadIN (TDA propiedad) X PropiedadOut (TDA propiedad)
% Recorrido: propiedad
propiedadDeshipotecar(Propiedad, PropiedadActualizada):-
    Propiedad = [ID, Nombre, Precio, Renta, Dueno, Casas, EsHotel, EstadoHipoteca], EstadoHipoteca \= false,
    propiedad(ID, Nombre, Precio, Renta, Dueno, Casas, EsHotel, false, PropiedadActualizada).