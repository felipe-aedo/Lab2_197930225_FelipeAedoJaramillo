% TDA jugador

:- module(tda_jugador, [
    jugador/8,
    esJugador/1,
    jugadorObtenerId/2,
    jugadorObtenerNombre/2,
    jugadorObtenerDinero/2,
    jugadorObtenerPropiedades/2,
    jugadorObtenerPosicion/2,
    jugadorEstaenCarcel/2,
    jugadorObtenerCartasCarcel/2,
    jugadorSetDinero/3,
    jugadorAgregarPropiedad/3,
    jugadorSetPosicion/3,
    jugadorSetEstaenCarcel/3,
    jugadorSetCartasCarcel/3,
    jugadorComprarPropiedad/3,
    jugadorPagarRenta/5
    ]).

:- use_module([operadores_aux, tda_propiedad]). %Importar libreria de operadores

%----CONSTRUCTOR----
% Descripcion: Constructor TDA jugador (formato lista)
% Dominio: ID (int) X Nombre (string) X Dinero (int) X Propiedades (list id's) X PosicionActual (int) X 
%          EstaEnCarcel (boolean) X CartasCarcel (int) X Jugador (TDA jugador)
% Recorrido: jugador
jugador(ID, Nombre, Dinero, Propiedades, Posicion, EstaenCarcel, CartasCarcel, Jugador):-
    Jugador = [ID, Nombre, Dinero, Propiedades, Posicion, EstaenCarcel, CartasCarcel].


%----PERTENENCIA----
% Descripcion: Verifica si es jugador valido
% Dominio: Jugador (TDA jugador)
% Recorrido: boolean
esJugador(Jugador):-
    Jugador = [ID, Nombre, Dinero, Propiedades, Posicion, EstaenCarcel, CartasCarcel],
    integer(ID), atom(Nombre), integer(Dinero), is_list(Propiedades), forall(member(P, Propiedades), integer(P)),integer(Posicion), boolean(EstaenCarcel), integer(CartasCarcel),
    Posicion >= 0, Dinero >= 0, ID > 0, CartasCarcel >= 0.


%------GETTERS------
% Descripcion: Obtiene id del jugador
% Dominio: Jugador (TDA jugador)
% Recorrido: integer
jugadorObtenerId(Jugador, ID):-
    Jugador = [ID, _, _, _, _, _, _].

% Descripcion: Obtiene nombre del jugador
% Dominio: Jugador (TDA jugador)
% Recorrido: atom
jugadorObtenerNombre(Jugador, Nombre):-
    Jugador = [_, Nombre, _, _, _, _, _].

% Descripcion: Obtiene dinero del jugador
% Dominio: Jugador (TDA jugador)
% Recorrido: integer
jugadorObtenerDinero(Jugador, Dinero):-
    Jugador = [_, _, Dinero, _, _, _, _].

% Descripcion: Obtiene propiedades del jugador
% Dominio: Jugador (TDA jugador)
% Recorrido: list
jugadorObtenerPropiedades(Jugador, Propiedades):-
    Jugador = [_, _, _, Propiedades, _, _, _].

% Descripcion: Obtiene posicion del jugador
% Dominio: Jugador (TDA jugador)
% Recorrido: integer
jugadorObtenerPosicion(Jugador, Posicion):-
    Jugador = [_, _, _, _, Posicion, _, _].

% Descripcion: Verifica si el jugador esta encarcelado
% Dominio: Jugador (TDA jugador) X EstaenCarcel (boolean)
% Recorrido: boolean
jugadorEstaenCarcel(Jugador, EstaenCarcel):-
    Jugador = [_, _, _, _, _, EstaenCarcel, _].

% Descripcion: Obtiene la cantidad de cartas para salir de la carcel
% Dominio: Jugador (TDA jugador) X CartasCarcel (int)
% Recorrido: integer
jugadorObtenerCartasCarcel(Jugador, CartasCarcel):-
    Jugador = [_, _, _, _, _, _, CartasCarcel].


%----SETTERS Y MODIFIERS----

% Descripcion: Modifica la cantidad de dinero que posee el jugador
% Dominio: Jugador (TDA jugador) X NuevoDinero (integer) X JugadorActualizado (TDA jugador)
% Recorrido: jugador
jugadorSetDinero(Jugador, NuevoDinero, JugadorActualizado):-
    Jugador = [ID, Nombre, _, Propiedades, Posicion, EstaenCarcel, CartasCarcel],
    JugadorActualizado = [ID, Nombre, NuevoDinero, Propiedades, Posicion, EstaenCarcel, CartasCarcel].

% Descripcion: Agrega una propiedad a la lista de propiedades del jugador
% Dominio: Jugador (TDA jugador) X NuevaPropiedad (TDA propiedad) X JugadorActualizado (TDA jugador)
% Recorrido: jugador
jugadorAgregarPropiedad(Jugador, NuevaPropiedad, JugadorActualizado):-
    Jugador = [ID, Nombre, Dinero, Propiedades, Posicion, EstaenCarcel, CartasCarcel],
    JugadorActualizado = [ID, Nombre, Dinero, [NuevaPropiedad|Propiedades], Posicion, EstaenCarcel, CartasCarcel].

% Descripcion: Modifica la posicion del jugador
% Dominio: Jugador (TDA jugador) X NuevaPosicion (integer) X JugadorActualizado (TDA jugador)
% Recorrido: jugador
jugadorSetPosicion(Jugador, NuevaPosicion, JugadorActualizado):-
    Jugador = [ID, Nombre, Dinero, Propiedades, _, EstaenCarcel, CartasCarcel],
    JugadorActualizado = [ID, Nombre, Dinero, Propiedades, NuevaPosicion, EstaenCarcel, CartasCarcel].

% Descripcion: Modifica el estado de encarcelamiento 
% Dominio: Jugador (TDA jugador) X EstadoCarcel (boolean) X JugadorActualizado (TDA jugador)
% Recorrido: jugador
jugadorSetEstaenCarcel(Jugador, EstadoCarcel, JugadorActualizado):-
    Jugador = [ID, Nombre, Dinero, Propiedades, Posicion, _, CartasCarcel],
    JugadorActualizado = [ID, Nombre, Dinero, Propiedades, Posicion, EstadoCarcel, CartasCarcel].

% Descripcion: Modifica la cantidad de cartas para ser desencarcelado
% Dominio: Jugador (TDA jugador) X NuevasCartasCarcel (integer) X JugadorActualizado (TDA jugador)
% Recorrido: jugador
jugadorSetCartasCarcel(Jugador, NuevasCartasCarcel, JugadorActualizado):-
    Jugador = [ID, Nombre, Dinero, Propiedades, Posicion, EstaenCarcel, _],
    JugadorActualizado = [ID, Nombre, Dinero, Propiedades, Posicion, EstaenCarcel, NuevasCartasCarcel].

% Descripcion: Compra propiedad verificando que alcance el dinero y la propiedad no tenga propietario
% Dominio: Jugador (TDA jugador) X Propiedad (TDA propiedad) X JugadorActualizado (TDA jugador)
% Recorrido: jugador
jugadorComprarPropiedad(Jugador, Propiedad, JugadorActualizado):-
    jugadorObtenerDinero(Jugador, Dinero), propiedadObtenerPrecio(Propiedad, Precio), Dinero >= Precio,
    propiedadObtenerDueno(Propiedad, Dueno), Dueno = [], DineroAct is Dinero - Precio, propiedadObtenerId(Propiedad, ID),
    jugadorSetDinero(Jugador, DineroAct, JugadorPaga), jugadorAgregarPropiedad(JugadorPaga, ID, JugadorActualizado).

% Descripcion: Transfiere dinero de un jugador a otro 
% Dominio: JugadorPagadorIN (TDA jugador) X JugadorReceptorIN (TDA jugador) X Monto (integer) X JugadorPagadorOUT (TDA jugador) X JugadorReceptorOUT (TDA jugador)
% Recorrido : JugadorPagadorActualizado X JugadorReceptorActualizado
% Caso 1: Puede pagar
jugadorPagarRenta(JugadorPagadorIN, JugadorReceptorIN, Monto, JugadorPagadorOUT, JugadorReceptorOUT) :-
    jugadorObtenerDinero(JugadorPagadorIN, DineroPagadorIN),
    jugadorObtenerDinero(JugadorReceptorIN, DineroReceptorIN),
    DineroPagadorOUT is DineroPagadorIN - Monto,
    DineroPagadorOUT >= 0,
    DineroReceptorOUT is DineroReceptorIN + Monto,
    jugadorSetDinero(JugadorPagadorIN, DineroPagadorOUT, JugadorPagadorOUT),
    jugadorSetDinero(JugadorReceptorIN, DineroReceptorOUT, JugadorReceptorOUT), !.
% Caso 2: No puede pagar (entrega todo lo que tiene)
jugadorPagarRenta(JugadorPagadorIN, JugadorReceptorIN, Monto, JugadorPagadorOUT, JugadorReceptorOUT) :- 
    jugadorObtenerDinero(JugadorPagadorIN, DineroPagadorIN),
    jugadorObtenerDinero(JugadorReceptorIN, DineroReceptorIN),
    Monto > DineroPagadorIN,
    DineroReceptorOUT is DineroReceptorIN + DineroPagadorIN,
    jugadorSetDinero(JugadorPagadorIN, 0, JugadorPagadorOUT),
    jugadorSetDinero(JugadorReceptorIN, DineroReceptorOUT, JugadorReceptorOUT), !.

% Descripcion: Verifica si un jugador esta en bancarrotra (Sin dinero).
% Dominio: Jugador (TDA jugador)
% Recorrido: boolean
jugadorEstaEnBancarrota(Jugador):-
    jugadorObtenerDinero(Jugador, Dinero), Dinero = 0.