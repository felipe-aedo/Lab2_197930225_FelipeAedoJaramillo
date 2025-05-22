%TDA juego

:- module(tda_juego, [
    juego/9,
    esJuego/1,
    juegoObtenerJugadores/2,
    juegoObtenerTablero/2,
    juegoObtenerDineroBanco/2,
    juegoObtenerNumeroDados/2,
    juegoObtenerTurnoActual/2,
    juegoAgregarJugador/3,
    juegoAgregarTablero/3,
    juegoObtenerJugadorActual/2,
    juegoLanzarDados/4,
    juegoMoverJugador/4
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

% Descripcion: Obtiene el player que coincide con el id
% Dominio: Juego (TDA juego) X ID (integer) X Jugador (TDA jugador)
% Recorrido: jugador
% Recursion natural
juegoObtenerJugador(Juego, ID, Jugador):- %Caso Exitoso
    juegoObtenerJugadores(Juego, [Jugador|_]), jugadorObtenerId(Jugador, ID), !.
juegoObtenerJugador(Juego, ID, Jugador):- %Caso recursivo
    juegoQuitarJugador(Juego, JuegoRest), juegoObtenerJugador(JuegoRest, ID, Jugador).

% Descripcion: Obtiene el jugador de turno
% Dominio: Juego (TDA juego) X Jugador(TDA jugador)
% Recorrido: jugador
juegoObtenerJugadorActual(Juego, JugadorActual):- %EL PRIMERO ES EL JUGADOR DE TURNO.
    juegoObtenerTurnoActual(Juego, Turno), 
    juegoObtenerJugador(Juego, Turno, JugadorActual).

% Descripcion: Obtiene la ultima posicion en el tablero del juego.
% Dominio: Juego (TDA juego) X Posicion (integer)
% Recorrido: integer
juegoObtenerUltimaPosicion(Juego, UltimaPosicion):-
    juegoObtenerTablero(Juego, Tablero),
    tableroObtenerUltimaPosicion(Tablero, UltimaPosicion).

%-----SETTERS Y MODIFIERS
% Descripcion: Agrega jugador al juego asigandole dinero del banco.
% Dominio: Juego (TDA juego) X Jugador (TDA jugador) X JuegoActualizado (TDA juego).
% Recorrido: juego
juegoAgregarJugador(Juego, Jugador, JuegoActualizado):-
    Juego = [_, Tablero, DineroBancoActual, NumeroDados, TurnoActual, TasaImpuesto, MaximoCasas, MaximoHoteles],
    juegoObtenerJugadores(Juego, JugadoresActuales),
    jugadorSetDinero(Jugador, 1500, JugadorActualizado),
    DineroBancoActual >= 1500 , DineroBancoActualizado is DineroBancoActual - 1500,
    juego([JugadorActualizado|JugadoresActuales], Tablero, DineroBancoActualizado, NumeroDados, TurnoActual, TasaImpuesto, MaximoCasas, MaximoHoteles, JuegoActualizado).

% Descripcion: Actualiza los jugadores en un juego
% Dominio: Juego (TDA juego) X Jugadores (lista de jugadores) X JuegoActualizado (TDA juego)
% Recorrido: juego
juegoActualizarJugadores(Juego, Jugadores, JuegoActualizado):-
    Juego = [_, Tablero, DineroBancoActual, NumeroDados, TurnoActual, TasaImpuesto, MaximoCasas, MaximoHoteles],
    juego(Jugadores, Tablero, DineroBancoActual, NumeroDados, TurnoActual, TasaImpuesto, MaximoCasas, MaximoHoteles, JuegoActualizado).

% Descripcion: Quita el primer jugador de la lista de jugadores del juego (util para iterar jugadores recursivamente).
% Dominio: Juego (TDA juego) X JuegoActualizado (TDA juego).
% Recorrido: juego
juegoQuitarJugador(Juego, JuegoActualizado):-
    Juego = [[_|RestoJugadores], Tablero, DineroBanco, NumeroDados, TurnoActual, TasaImpuesto, MaximoCasas, MaximoHoteles],
    JuegoActualizado = [RestoJugadores, Tablero, DineroBanco, NumeroDados, TurnoActual, TasaImpuesto, MaximoCasas, MaximoHoteles].

% Descripcion: Actualiza un jugador en el juego.
% Dominio: Juego (TDA juego) X Jugador (TDA jugador) X JuegoActualizado (TDA juego)
% Recorrido: juego
juegoActualizarJugador(Juego, Jugador, JuegoActualizado):-
    juegoObtenerJugadores(Juego, Jugadores),
    actualizarJugador(Jugadores, Jugador, JugadoresActualizados),
    juegoActualizarJugadores(Juego, JugadoresActualizados, JuegoActualizado).

% Descripcion: Actualiza un jugador en una lista de jugadores
% Dominio: lista de jugadores X jugador X lista actualizada
% Recorrido: lista de jugadores
actualizarJugador([], _, []).
actualizarJugador([JActual | Resto], JNuevo, [JNuevo | Resto]) :-
    jugadorObtenerId(JActual, Id1),
    jugadorObtenerId(JNuevo, Id2),
    Id1 =:= Id2, !.
actualizarJugador([JActual | Resto], JNuevo, [JActual | RestoActualizado]) :-
    actualizarJugador(Resto, JNuevo, RestoActualizado).
  

% Descripcion: Agrega tablero al juego.
% Dominio: Juego (TDA juego) X Tablero (TDA tablero) X JuegoActualizado (TDA juego).
% Recorrido: juego
juegoAgregarTablero(Juego, Tablero, JuegoActualizado):-
    Juego = [Jugadores, _, DineroBanco, NumeroDados, TurnoActual, TasaImpuesto, MaximoCasas, MaximoHoteles],
    JuegoActualizado = [Jugadores, Tablero, DineroBanco, NumeroDados, TurnoActual, TasaImpuesto, MaximoCasas, MaximoHoteles].

% Descripcion: Mueve al jugador en el tablero actualizando el juego.
% Dominio: Juego (TDA juego) X ID (integer) X Dados (lista integer) X JuegoActualizado (TDA juego)
% Recorrido: juego
juegoMoverJugador(Juego, ID, Dados, JuegoActualizado):-
    juegoObtenerJugador(Juego, ID, Jugador),
    juegoObtenerUltimaPosicion(Juego, UltimaPosicion),
    jugadorObtenerPosicion(Jugador, Posicion),
    sumarLista(Dados, Movimiento),
    PosicionActualizada is Posicion + Movimiento,
    jugadorSetPosicion(Jugador, PosicionActualizada, JugadorPre),
    jugadorCorregirPosicion(JugadorPre, UltimaPosicion, JugadorActualizado),
    juegoActualizarJugador(Juego, JugadorActualizado, JuegoActualizado).

% Descripcion: Juega la cantidad de dados indicadas en el TDA juego.
% Dominio: Juego (TDA juego) X Seeds (lista de integer) X NuevasSeeds (lista de integer) X Resultados (lista de integer)
% Recorrido: lista de seeds restantes X lista de dados.
% Recursión de cola
juegoLanzarDados(Juego, SeedDados, NSeedDados, ResultadoDados):-
    juegoObtenerNumeroDados(Juego, NDados),
    lanzar_dados(SeedDados, NDados, NSeedDados, ResultadoDados).

%Predicado auxiliar para iterar cada cado a generar
% Caso base: sin dados
lanzar_dados([], 0, [], []).
% Caso recursivo: usar una seed, repetir
lanzar_dados([Seed | RestoSeeds], N, [NvaSeed | NuevasSeeds], [R | Resultados]):-
    N > 0,
    getDadoRandom(Seed, NvaSeed, R),
    N1 is N - 1,
    lanzar_dados(RestoSeeds, N1, NuevasSeeds, Resultados).

% Predicado myRandom
myRandom(Xn, Xn1):-
  Xn1 is ((1103515245 * Xn) + 12345) mod 2147483648.

% Predicado getDadoRandom que recibe la semilla y controla los resultados
getDadoRandom(Seed, NvaSeed, R):-
    myRandom(Seed, NvaSeed),
    R is 1 + (NvaSeed mod 6).