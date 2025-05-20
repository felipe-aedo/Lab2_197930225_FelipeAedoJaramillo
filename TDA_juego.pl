%TDA juego

:- module(tda_juego, [
    juego/9,
    esJuego/1,
    juegoObtenerJugadores/2,
    juegoObtenerTablero/2,
    juegoObtenerDineroBanco/2,
    juegoObtenerNumeroDados/2,
    juegoObtenerTurnoActual/2,
    juegoAgregarJugador/3
    ]).

:- use_module([tda_jugador, tda_propiedad, tda_tablero, tda_carta, operadores_aux]).

%-----CONSTRUCTOR-----
% Descripcion: Constructor TDA Juego
% Dominio: Jugadores (lista de jugadores) X Tablero (TDA tablero) X DineroBanco (integer) X NumeroDados (integer) X TurnoActual (integer) X TasaImpuesto (integer)
%                    X MaximoCasas (integer) X MaximoHoteles (integer) X Juego (TDA juego).
% Recorrido: juego
juego(Jugadores, Tablero, DineroBanco, NumeroDados, TurnoActual, TasaImpuesto, MaximoCasas, MaximoHoteles, Juego):-
    Juego = [Jugadores, Tablero, DineroBanco, NumeroDados, TurnoActual, TasaImpuesto, MaximoCasas, MaximoHoteles].

%----PERTENENCIA----
% Descripcion: Verifica si es un juego valido
% Dominio: Juego (TDA juego)
% Recorrido: boolean
esJuego(Juego):-
    Juego = [Jugadores, Tablero, DineroBanco, NumeroDados, TurnoActual, TasaImpuesto, MaximoCasas, MaximoHoteles],
    is_list(Jugadores),
    is_list(Tablero),
    integer(DineroBanco), integer(NumeroDados), integer(TurnoActual),
    integer(TasaImpuesto), integer(MaximoCasas), integer(MaximoHoteles),
    DineroBanco >= 0, NumeroDados > 0, TurnoActual >= 0,
    TasaImpuesto >= 0, MaximoCasas >= 0, MaximoHoteles >= 0.

%------GETTERS------
% Descripcion: Obtiene la lista de jugadores del juego
% Dominio: Juego (TDA juego)
% Recorrido: list
juegoObtenerJugadores(Juego, Jugadores):-
    Juego = [Jugadores, _, _, _, _, _, _, _].

% Descripcion: Obtiene el tablero del juego
% Dominio: Juego (TDA juego)
% Recorrido: list
juegoObtenerTablero(Juego, Tablero):-
    Juego = [_, Tablero, _, _, _, _, _, _].

% Descripcion: Obtiene el dinero del banco
% Dominio: Juego (TDA juego)
% Recorrido: integer
juegoObtenerDineroBanco(Juego, DineroBanco):-
    Juego = [_, _, DineroBanco, _, _, _, _, _].

% Descripcion: Obtiene el numero de dados del juego
% Dominio: Juego (TDA juego)
% Recorrido: integer
juegoObtenerNumeroDados(Juego, NumeroDados):-
    Juego = [_, _, _, NumeroDados, _, _, _, _].

% Descripcion: Obtiene el turno actual
% Dominio: Juego (TDA juego)
% Recorrido: integer
juegoObtenerTurnoActual(Juego, TurnoActual):-
    Juego = [_, _, _, _, TurnoActual, _, _, _].

% Descripcion: Obtiene la tasa de impuesto
% Dominio: Juego (TDA juego)
% Recorrido: integer
juegoObtenerTasaImpuesto(Juego, TasaImpuesto):-
    Juego = [_, _, _, _, _, TasaImpuesto, _, _].

% Descripcion: Obtiene el maximo de casas por propiedad
% Dominio: Juego (TDA juego)
% Recorrido: integer
juegoObtenerMaximoCasas(Juego, MaximoCasas):-
    Juego = [_, _, _, _, _, _, MaximoCasas, _].

% Descripcion: Obtiene el maximo de hoteles por propiedad
% Dominio: Juego (TDA juego)
% Recorrido: integer
juegoObtenerMaximoHoteles(Juego, MaximoHoteles):-
    Juego = [_, _, _, _, _, _, _, MaximoHoteles].

% Descripcion: Agrega jugador al juego asigandole dinero del banco.
% Dominio: Juego (TDA juego) X Jugador (TDA jugador) X JuegoActualizado (TDA juego).
% Recorrido: juego
juegoAgregarJugador(Juego, Jugador, JuegoActualizado):-
    Juego = [_, Tablero, DineroBancoActual, NumeroDados, TurnoActual, TasaImpuesto, MaximoCasas, MaximoHoteles],
    juegoObtenerJugadores(Juego, JugadoresActuales),
    DineroBancoActual >= 1500 , DineroBancoActualizado is DineroBancoActual - 1500,
    jugador([Jugador|JugadoresActuales], Tablero, DineroBancoActualizado, NumeroDados, TurnoActual, TasaImpuesto, MaximoCasas, MaximoHoteles, JuegoActualizado).