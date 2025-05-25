%TDA juego

:- module(tda_juego, [
    juego/9,
    esJuego/1,
    juegoObtenerJugadores/2,
    juegoObtenerTablero/2,
    juegoObtenerDineroBanco/2,
    juegoObtenerNumeroDados/2,
    juegoObtenerTurnoActual/2,
    juegoObtenerJugadorActual/2,
    juegoObtenerPosicion/3,
    juegoAgregarJugador/3,
    juegoAgregarTablero/3,
    juegoActualizarJugadores/3,
    juegoActualizarPropiedad/3,
    juegoLanzarDados/4,
    juegoMoverJugador/4,
    juegoConstruirCasa/3,
    juegoCalcularRentaPropiedad/3,
    juegoCalcularRentaJugador/3,
    juegoExtraerCarta/6
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

% Descripcion: Obtiene la propiedad de id especifico
% Dominio: Juego (TDA juego) X IdPropiedad (integer) X Propiedad (TDA propiedad)
% Recorrido: propiedad
juegoObtenerPropiedad(Juego, IdPropiedad, Propiedad):-
    juegoObtenerTablero(Juego, Tablero),
    tableroObtenerPropiedad(Tablero, IdPropiedad, Propiedad).

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

% Descripcion: Obtiene el mayor id entre los jugadores
% Dominio: Juego (TDA juego) X UltimoTurno (integer)
% Recorrido: integer
juegoObtenerUltimoTurno(Juego, UltimoTurno):-
    juegoObtenerJugadores(Juego,Jugadores),
    ultimoTurno(Jugadores, 0, UltimoTurno).
ultimoTurno([], UltimoTurno, UltimoTurno):- !.
ultimoTurno([Jugador|ColaJugadores], TurnoActual, UltimoTurno):-
    jugadorObtenerId(Jugador, ID),
    max(TurnoActual, ID, TurnoIntermedio),
    ultimoTurno(ColaJugadores, TurnoIntermedio, UltimoTurno).

% Descripcion: Obtiene la ultima posicion en el tablero del juego.
% Dominio: Juego (TDA juego) X Posicion (integer)
% Recorrido: integer
juegoObtenerUltimaPosicion(Juego, UltimaPosicion):-
    juegoObtenerTablero(Juego, Tablero),
    tableroObtenerUltimaPosicion(Tablero, UltimaPosicion).

% Descripcion: Obtiene el elemento en la posicion especificada (propiedad o casilla especial)
% Dominio : Tablero (TDA Juego) X Posicion (integer) X Elemento (Propiedad o casillaEspecial)
% Recorrido : casilla o propiedad
juegoObtenerPosicion(Juego, Posicion, Elemento):-
    juegoObtenerTablero(Juego, Tablero),
    tableroObtenerPosicion(Tablero, Posicion, Elemento).

% Descripcion: Obtiene las cartas de un tipo especificado
% Dominio: Juego (TDA juego) X TipoCarta (Atom) X Cartas (Lista de cartas)
% Recorrido: lista de cartas
juegoObtenerCartas(Juego, TipoCarta, Cartas):-
    juegoObtenerTablero(Juego, Tablero),
    tableroObtenerCartas(Tablero, TipoCarta, Cartas).

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
% Recursion natural
actualizarJugador([], _, []).
actualizarJugador([JActual | Resto], JNuevo, [JNuevo | Resto]) :-
    jugadorObtenerId(JActual, Id1),
    jugadorObtenerId(JNuevo, Id2),
    Id1 =:= Id2, !.
actualizarJugador([JActual | Resto], JNuevo, [JActual | RestoActualizado]) :-
    actualizarJugador(Resto, JNuevo, RestoActualizado).

% Descripcion: Actualiza una propiedad en el juego
% Dom: Juego (TDA juego) X Propiedad (TDA propiedad) X JuegoActualizado (TDA juego)
% Rec: juego
juegoActualizarPropiedad(Juego, Propiedad, JuegoActualizado):-
    juegoObtenerTablero(Juego, Tablero),
    tableroActualizarPropiedad(Tablero, Propiedad, TableroActualizado),
    juegoAgregarTablero(Juego, TableroActualizado, JuegoActualizado).

% Descripcion: Agrega tablero al juego.
% Dominio: Juego (TDA juego) X Tablero (TDA tablero) X JuegoActualizado (TDA juego).
% Recorrido: juego
juegoAgregarTablero(Juego, Tablero, JuegoActualizado):-
    Juego = [Jugadores, _, DineroBanco, NumeroDados, TurnoActual, TasaImpuesto, MaximoCasas, MaximoHoteles],
    juego(Jugadores, Tablero, DineroBanco, NumeroDados, TurnoActual, TasaImpuesto, MaximoCasas, MaximoHoteles, JuegoActualizado).

% Descripcion: Actualiza el dinero en el banco.
% Dominio: Juego (TDA juego) X Dinero (integer) X JuegoActualizado (TDA juego).
% Recorrido: juego
juegoSetDineroBanco(Juego, Dinero, JuegoActualizado):-
    Juego = [Jugadores, Tablero, _, NumeroDados, TurnoActual, TasaImpuesto, MaximoCasas, MaximoHoteles],
    juego(Jugadores, Tablero, Dinero, NumeroDados, TurnoActual, TasaImpuesto, MaximoCasas, MaximoHoteles, JuegoActualizado).

% Descripcion: Actualiza el turno.
% Dominio: Juego (TDA juego) X Turno (integer) X JuegoActualizado (TDA juego).
% Recorrido: juego
juegoSetTurnoActual(Juego, Turno, JuegoActualizado):-
    Juego = [Jugadores, Tablero, DineroBanco, NumeroDados, _, TasaImpuesto, MaximoCasas, MaximoHoteles],
    juego(Jugadores, Tablero, DineroBanco, NumeroDados, Turno, TasaImpuesto, MaximoCasas, MaximoHoteles, JuegoActualizado).

% Descripcion: Actualiza la tasa de impuesto del juego.
% Dominio: Juego (TDA juego) X TasaImpuesto (integer) X JuegoActualizado (TDA juego).
% Recorrido: juego
juegoSetTasaImpuesto(Juego, TasaImpuesto, JuegoActualizado):-
    Juego = [Jugadores, Tablero, Dinero, NumeroDados, TurnoActual, _, MaximoCasas, MaximoHoteles],
    juego(Jugadores, Tablero, Dinero, NumeroDados, TurnoActual, TasaImpuesto, MaximoCasas, MaximoHoteles, JuegoActualizado).

% Descripcion: Compra una propiedad el jugador de turno.
% Dominio: Juego (TDA juego)  X JuegoActualizado (TDA juego)
% Recorrido : juego
juegoComprarPropiedad(Juego, JuegoActualizado):-
    juegoObtenerJugadorActual(Juego, Jugador),
    jugadorObtenerPosicion(Jugador, Posicion),
    juegoObtenerPosicion(Juego, Posicion, Propiedad),
    esPropiedad(Propiedad),
    jugadorObtenerDinero(Jugador, Dinero1),
    juegoObtenerDineroBanco(Juego, DineroBanco),
    jugadorComprarPropiedad(Jugador, Propiedad, JugadorActualizado),
    jugadorObtenerDinero(Jugador, Dinero2),
    VerificaCompra is Dinero1 - Dinero2, %Si no se compro la propiedad, no cambia el dinero del banco
    DineroBancoActualizado is DineroBanco + VerificaCompra,
    juegoSetDineroBanco(Juego, DineroBancoActualizado, JuegoBancoActualizado),
    juegoActualizarJugador(JuegoBancoActualizado, JugadorActualizado, JuegoActualizado).

% Descripcion: Actualiza una propiedad en el juego construyendo una casa en ella
% Dom: Juego (TDA juego) X Propiedad (TDA propiedad) X JuegoActualizado (TDA juego)
% Rec: juego
juegoConstruirCasa(Juego, Propiedad, JuegoActualizado):-
    juegoObtenerMaximoCasas(Juego, MaximoCasas),
    propiedadObtenerCasas(Propiedad, Casas),
    Casas < MaximoCasas, NCasas is Casas + 1,
    propiedadSetCasas(Propiedad, NCasas, PropiedadActualizada),
    juegoActualizarPropiedad(Juego, PropiedadActualizada, JuegoActualizado).
juegoConstruirCasa(Juego, Propiedad, Juego):- %no es posible construir
    juegoObtenerMaximoCasas(Juego, MaximoCasas),
    propiedadObtenerCasas(Propiedad, Casas),
    Casas = MaximoCasas.

% Descripcion: Actualiza una propiedad en el juego construyendo un hotel
% Dom: Juego (TDA juego) X Propiedad (TDA propiedad) X JuegoActualizado (TDA juego)
% Rec: juego
juegoConstruirHotel(Juego, Propiedad, JuegoActualizado):-
    juegoObtenerMaximoCasas(Juego, MaximoCasas),
    propiedadObtenerCasas(Propiedad, Casas),
    Casas = MaximoCasas,
    \+ propiedadEsHotel(Propiedad),
    propiedadSetHotel(Propiedad, true, PropiedadConHotel),
    propiedadSetCasas(PropiedadConHotel, 0, PropiedadActualizada),
    juegoActualizarPropiedad(Juego, PropiedadActualizada, JuegoActualizado).
juegoConstruirHotel(Juego, Propiedad, Juego):- %no es posible construir
    juegoObtenerMaximoCasas(Juego, MaximoCasas),
    propiedadObtenerCasas(Propiedad, Casas),
    (Casas \= MaximoCasas ; propiedadEsHotel(Propiedad)).

% Descripcion: Calcula la renta de una propiedad
% Dominio: Juego (TDA juego) X propiedad (TDA propiedad) X Monto (number)
% Recorrido : integer
juegoCalcularRentaPropiedad(_, Propiedad, Monto):-% Caso : No es hotel
    \+ propiedadEsHotel(Propiedad),
    propiedadObtenerCasas(Propiedad, CantidadCasas),
    propiedadObtenerRenta(Propiedad, RentaBase),
    RentaCasas is RentaBase* 0.2 * CantidadCasas,
    Monto is RentaCasas + RentaBase.
juegoCalcularRentaPropiedad(Juego, Propiedad, Monto):- %Caso : Es Hotel
    propiedadEsHotel(Propiedad),
    juegoObtenerMaximoCasas(Juego, MaximoCasas),
    propiedadObtenerRenta(Propiedad, RentaBase),
    MontoParcial is RentaBase * 0.2 * MaximoCasas,
    Monto is MontoParcial + RentaBase.

% Descripcion: Calcula la renta de un jugador (la suma de todas sus rentas)
% Dominio: Juego (TDA juego) X Jugador (TDA jugador) X renta (number)
% Recorrido : integer
juegoCalcularRentaJugador(Juego, Jugador, Renta):-
    jugadorObtenerPropiedades(Jugador, Propiedades),
    acumularRentas(Juego, Propiedades, Renta).
% Descripcion: Acumula las rentas de una lista de propiedades 
% Recursion natural
acumularRentas(_, [], 0):- !.
acumularRentas(Juego, [IdPropiedad|RestoProps], RentaTotal):-
    juegoObtenerPropiedad(Juego, IdPropiedad, Propiedad),
    juegoCalcularRentaPropiedad(Juego, Propiedad, Renta),
    acumularRentas(Juego, RestoProps, RestoRenta),
    RentaTotal is Renta + RestoRenta.

% Descripcion: Cobra dinero a un jugador para ser guardado en el banco.
% Dominio: Juego (TDA juego) X Jugador (TDA jugador) X Monto (integer) X JuegoActualizado (TDA juego).
% Recorrido: juego
juegoCobrarJugador(Juego, Jugador, Monto, JuegoActualizado):-
    juegoObtenerDineroBanco(Juego, DineroBanco),
    jugadorObtenerDinero(Jugador, DineroJugador),
    DineroJugador >= Monto, 
    DineroJugadorActualizado is DineroJugador - Monto,
    DineroBancoActualizado is DineroBanco + Monto,
    juegoSetDineroBanco(Juego, DineroBancoActualizado, JuegoBancoActualizado),
    jugadorSetDinero(Jugador, DineroJugadorActualizado, JugadorActualizado),
    juegoActualizarJugador(JuegoBancoActualizado, JugadorActualizado, JuegoActualizado).

% Descripcion: Mueve al jugador en el tablero actualizando el juego.
% Dominio: Juego (TDA juego) X ID (integer) X Dados (lista integer) X JuegoActualizado (TDA juego)
% Recorrido: juego
juegoMoverJugador(Juego, ID, Dados, JuegoActualizado):-
    juegoObtenerJugador(Juego, ID, Jugador),
    juegoObtenerUltimaPosicion(Juego, UltimaPosicion),
    jugadorObtenerPosicion(Jugador, Posicion),
    sumarLista(Dados, Movimiento),
    PosicionActualizada is Posicion + Movimiento,
    jugadorSetPosicion(Jugador, PosicionActualizada, JugadorPre), %Necesario cobrar impuesto antes de corregir posicion
    juegoImpuestoPropiedades(Juego, JugadorPre, JuegoParcial),
    jugadorCorregirPosicion(JugadorPre, UltimaPosicion, JugadorActualizado),
    juegoActualizarJugador(JuegoParcial, JugadorActualizado, JuegoActualizado).

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

% Descripcion: Cobra impuesto por propiedad al jugador
% Dominio: Juego (TDA juego) X Jugador (TDA jugador) X JuegoActualizado (TDA juego)
% Recorrido: juego
juegoImpuestoPropiedades(Juego, Jugador, JuegoActualizado):-
    juegoCalcularRentaJugador(Juego, Jugador, Renta),
    juegoObtenerTasaImpuesto(Juego, TasaImpuesto),
    Cobro is (TasaImpuesto * Renta) // 100,
    juegoCobrarJugador(Juego, Jugador, Cobro, JuegoActualizado).

% Descripcion: Obtiene una carta aleatoria de un tipo especificado
% Dominio: Juego (TDA juego) X TipoCarta (Atom) X Seed (integer) X NuevaSeed (integer) X Carta (TDA carta)
% Recorrido: JuegoActualizado X Carta
juegoExtraerCarta(Juego, TipoCarta, [Seed], [NuevaSeed], JuegoActualizado, Carta) :-
    juegoObtenerCartas(Juego, TipoCarta, Cartas),
    length(Cartas, CantidadCartas),
    getDadoRandom(Seed, NuevaSeed, R),
    Index is ((R - 1) mod CantidadCartas) + 1,
    nth1(Index, Cartas, Carta),
    juegoEjecutarCarta(Juego, Carta, JuegoActualizado).

% Descripcion: Ejecuta la accion de una carta en un juego.
% Dominio: Juego (TDA juego) X Carta (TDA carta) X JuegoActualizado (TDA juego)
% Recorrido : juego
juegoEjecutarCarta(Juego, Carta, JuegoActualizado):-
    cartaObtenerAccion(Carta, Accion),
    call(Accion, Juego, JuegoActualizado).

% Descripcion: Avanza de turno el juego.
% Dominio: Juego (TDA juego) X Juego (TDA juego)
% Recorrido: juego
juegoAvanzarTurno(Juego, JuegoActualizado):-
    juegoObtenerTurnoActual(Juego,TurnoActual),
    juegoObtenerUltimoTurno(Juego, UltimoTurno),
    TurnoActual < UltimoTurno,
    TurnoActualizado is TurnoActual + 1,
    juegoSetTurnoActual(Juego, TurnoActualizado, JuegoActualizado).
juegoAvanzarTurno(Juego, JuegoActualizado):-
    juegoObtenerTurnoActual(Juego,TurnoActual),
    juegoObtenerUltimoTurno(Juego, UltimoTurno),
    TurnoActual = UltimoTurno,
    juegoSetTurnoActual(Juego, 1, JuegoActualizado).

%juegoJugarTurno()

% ------Acciones de cartas (tipo suerte) --------
% Descripcion: Mueve al jugador actual hasta la salida
% Dominio: Juego (TDA juego) X JuegoActualizado (TDA juego)
% Recorrido : juego
irASalida(Juego, JuegoActualizado):-
    juegoObtenerJugadorActual(Juego, Jugador),
    jugadorSetPosicion(Jugador, 0 , JugadorActualizado),
    juegoActualizarJugador(Juego, JugadorActualizado, JuegoActualizado).

% Descripcion: Encarcela jugador actual
% Dominio: Juego (TDA juego)  X JuegoActualizado (TDA juego)
% Recorrido : juego
irACarcel(Juego, JuegoActualizado):-
    juegoObtenerTablero(Juego, Tablero),
    tableroObtenerCarcel(Tablero, PosCarcel),
    juegoObtenerJugadorActual(Juego, Jugador),
    jugadorSetPosicion(Jugador, PosCarcel, JugadorCarcel),
    jugadorSetEstaenCarcel(JugadorCarcel, true, JugadorActualizado),
    juegoActualizarJugador(Juego, JugadorActualizado, JuegoActualizado).

% Descripcion: Agrega 20000 de dinero al jugador actual
% Dominio: Juego (TDA juego)  X JuegoActualizado (TDA juego)
% Recorrido : juego
ganarKino(Juego, JuegoActualizado):-
    juegoObtenerJugadorActual(Juego, Jugador),
    jugadorObtenerDinero(Jugador, Dinero),
    DineroActualizado is Dinero + 20000,
    jugadorSetDinero(Jugador, DineroActualizado, JugadorActualizado),
    juegoActualizarJugador(Juego, JugadorActualizado, JuegoActualizado).

%----Comunidad-----
% Descripcion: Paga el 10% de su dinero al banco
% Dominio: Juego (TDA juego)  X JuegoActualizado (TDA juego)
% Recorrido : juego
pagarImpuesto(Juego, JuegoActualizado):-
    juegoObtenerJugadorActual(Juego, Jugador),
    jugadorObtenerDinero(Jugador, Dinero),
    Cobro is Dinero * 0.1, CobroAprox is round(Cobro),
    juegoCobrarJugador(Juego, Jugador, CobroAprox, JuegoActualizado).

% Descripcion: Cambia el impuesto del juego siguiendo este orden (5% - 10% - 15%)
% Dominio: Juego (TDA juego)  X JuegoActualizado (TDA juego)
% Recorrido : juego
cambiarImpuesto(Juego, JuegoActualizado):-
    juegoObtenerTasaImpuesto(Juego, TasaActual),
    TasaActual = 10, TasaActualizada is 15,
    juegoSetTasaImpuesto(Juego, TasaActualizada, JuegoActualizado).
cambiarImpuesto(Juego, JuegoActualizado):-
    juegoObtenerTasaImpuesto(Juego, TasaActual),
    TasaActual = 15, TasaActualizada is 5,
    juegoSetTasaImpuesto(Juego, TasaActualizada, JuegoActualizado).
cambiarImpuesto(Juego, JuegoActualizado):-
    juegoObtenerTasaImpuesto(Juego, TasaActual),
    TasaActual = 5, TasaActualizada is 10,
    juegoSetTasaImpuesto(Juego, TasaActualizada, JuegoActualizado).    