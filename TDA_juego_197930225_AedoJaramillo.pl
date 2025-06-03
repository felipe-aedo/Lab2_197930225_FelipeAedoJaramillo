%TDA juego

:- module(tda_juego_197930225_AedoJaramillo, [
    juego/9,
    esJuego/1,
    juegoComenzar/2,
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
    juegoAvanzarTurno/2,
    juegoLanzarDados/4,
    juegoMoverJugador/4,
    juegoAccionComprarPropiedad/3,
    juegoComprarPropiedad/2,
    juegoConstruirCasa/3,
    juegoCalcularRentaPropiedad/3,
    juegoCalcularRentaJugador/3,
    juegoHipotecarPropiedad/3,
    juegoExtraerCarta/6,
    juegoJugarTurno/6
    ]).

:- use_module([tda_jugador_197930225_AedoJaramillo, 
    tda_propiedad_197930225_AedoJaramillo, 
    tda_tablero_197930225_AedoJaramillo, 
    tda_carta_197930225_AedoJaramillo, 
    operadores_aux_197930225_AedoJaramillo
    ]).

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
    DineroBanco >= 0, NumeroDados > 0, TurnoActual >= -1,
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
    juegoObtenerJugador(Juego, Turno, JugadorActual), !.

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

% Descripcion: Obtiene la ultima posicion (integer) en el tablero del juego.
% Dominio: Juego (TDA juego) X Posicion (integer)
% Recorrido: integer
juegoObtenerUltimaPosicion(Juego, UltimaPosicion):-
    juegoObtenerTablero(Juego, Tablero),
    tableroObtenerUltimaPosicion(Tablero, UltimaPosicion), !.

% Descripcion: Obtiene el elemento en la posicion especificada (propiedad o casilla especial)
% Dominio : Tablero (TDA Juego) X Posicion (integer) X Elemento (Propiedad o casillaEspecial)
% Recorrido : casilla o propiedad
juegoObtenerPosicion(Juego, Posicion, Elemento):-
    juegoObtenerTablero(Juego, Tablero),
    tableroObtenerPosicion(Tablero, Posicion, Elemento).

% Descripcion: Obtiene la posicion (casilla o propiedad) del player de turno.
% Dominio: Juego (TDA juego) X Elemento(Propiedad o CasillaEspecial)
% Recorrido: casilla
juegoObtenerCasillaActual(Juego, Elemento):-
    juegoObtenerJugadorActual(Juego, JugadorActual),
    jugadorObtenerPosicion(JugadorActual, Posicion),
    juegoObtenerPosicion(Juego, Posicion, Elemento).

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

% Descripcion: Paga jugador al jugador asigandole dinero del banco.
% Dominio: Juego (TDA juego) X Jugador (TDA jugador) X JuegoActualizado (TDA juego).
% Recorrido: juego
juegoPagarJugador(Juego, Jugador, Monto, JuegoActualizado):-
    juegoObtenerDineroBanco(Juego, DineroBanco), jugadorObtenerDinero(Jugador, DineroJugador),
    Monto < DineroBanco, DineroBancoActualizado is DineroBanco - Monto, DineroJugadorActualizado is DineroJugador + Monto,
    juegoSetDineroBanco(Juego, DineroBancoActualizado, JuegoBancoActualizado),
    jugadorSetDinero(Jugador, DineroJugadorActualizado, JugadorActualizado),
    juegoActualizarJugador(JuegoBancoActualizado, JugadorActualizado, JuegoActualizado), !.
juegoPagarJugador(Juego, _, _, Juego):- write('\nFallo al pagar (Banco sin dinero)\n\n'), !.

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


%Accion para el predicado de comprar propiedad (Para facilitar su uso en juegoJugarTurno)
juegoAccionComprarPropiedad(Juego, _, JuegoActualizado):- juegoComprarPropiedad(Juego,JuegoActualizado).
juegoAccionComprarPropiedad(Juego, _, Juego):- !.
% Descripcion: Compra una propiedad el jugador de turno.
% Dominio: Juego (TDA juego)  X JuegoActualizado (TDA juego)
% Recorrido : juego
juegoComprarPropiedad(Juego, JuegoActualizado):-
    juegoObtenerJugadorActual(Juego, Jugador),
    jugadorObtenerPosicion(Jugador, Posicion),
    juegoObtenerPosicion(Juego, Posicion, Propiedad),
    esPropiedad(Propiedad),
    propiedadObtenerPrecio(Propiedad, Precio),
    propiedadObtenerNombre(Propiedad, NombreProp),
    jugadorObtenerDinero(Jugador, Dinero),
    Dinero > Precio, %Debe poder comprar
    jugadorComprarPropiedad(Jugador, Propiedad, JugadorActualizado),
    jugadorObtenerId(Jugador, ID),
    propiedadSetDueno(Propiedad, ID, PropiedadActualizada),
    juegoCobrarJugador(Juego, JugadorActualizado, Precio, JuegoA1),
    juegoActualizarPropiedad(JuegoA1, PropiedadActualizada, JuegoActualizado),
    write('La propiedad '), write(NombreProp), write(' ha sido comprada por el jugador '), writeln(ID).

% Descripcion: Construye una casa para la propiedad del jugador actual. actualiza jugador y propiedad
% Dom: Juego (TDA juego) X IdPropiedad (integer) X JuegoActualizado (TDA juego)
% Rec: juego
juegoConstruirCasa(Juego, IdPropiedad, JuegoActualizado):-
    juegoObtenerJugadorActual(Juego, Jugador),
    juegoObtenerPropiedad(Juego, IdPropiedad, Propiedad),
    jugadorPoseePropiedad(Jugador, IdPropiedad),
    juegoObtenerMaximoCasas(Juego, MaximoCasas),
    jugadorObtenerDinero(Jugador, Dinero),
    propiedadObtenerCasas(Propiedad, Casas),
    propiedadObtenerRenta(Propiedad, Precio), %se modifico para que las casas sean mas asequibles
    Dinero >= Precio,
    Casas < MaximoCasas, NCasas is Casas + 1,
    propiedadSetCasas(Propiedad, NCasas, PropiedadActualizada),
    juegoCobrarJugador(Juego, Jugador, Precio, Juego1),
    juegoActualizarPropiedad(Juego1, PropiedadActualizada, JuegoActualizado), 
    write('Casa construida en propiedad '), writeln(IdPropiedad), !.
juegoConstruirCasa(Juego, _, Juego):- !. %no es posible construir

% Descripcion: Construye un hotel para la propiedad del jugador actual. actualiza propiedad
% Dom: Juego (TDA juego) X IdPropiedad (integer) X JuegoActualizado (TDA juego)
% Rec: juego
juegoConstruirHotel(Juego, IdPropiedad, JuegoActualizado):-
    juegoObtenerJugadorActual(Juego, Jugador),
    jugadorPoseePropiedad(Jugador, IdPropiedad),
    juegoObtenerPropiedad(Juego, IdPropiedad, Propiedad),
    juegoObtenerMaximoCasas(Juego, MaximoCasas),
    propiedadObtenerCasas(Propiedad, Casas),
    Casas = MaximoCasas,
    \+ propiedadEsHotel(Propiedad),
    propiedadSetHotel(Propiedad, true, PropiedadConHotel),
    propiedadSetCasas(PropiedadConHotel, 0, PropiedadActualizada),
    juegoActualizarPropiedad(Juego, PropiedadActualizada, JuegoActualizado), 
    writeln('Hotel construido en propiedad '), writeln(IdPropiedad) ,!.
juegoConstruirHotel(Juego, _, Juego):- !. % no es posible construir


% Descripcion: Hipoteca una propiedad en el juego
% Dominio: Juego (TDA juego) X IdPropiedad (integer) X JuegoActualizado (TDA juego)
% Recorrido: juego
juegoHipotecarPropiedad(Juego, IdPropiedad, JuegoActualizado) :-
    juegoObtenerJugadorActual(Juego, JugadorActual),
    juegoObtenerPropiedad(Juego, IdPropiedad, Propiedad),
    jugadorPoseePropiedad(JugadorActual, IdPropiedad),
    \+ propiedadEstaHipotecada(Propiedad), % Hipotecar propiedad y actualizarla en el juego
    propiedadHipotecar(Propiedad, PropHipotecada),
    juegoActualizarPropiedad(Juego, PropHipotecada, JuegoA1),
    propiedadObtenerPrecio(Propiedad, Precio),
    MontoHipoteca is Precio // 2,
    juegoPagarJugador(JuegoA1, JugadorActual, MontoHipoteca, JuegoActualizado),
    write('La propiedad '), write(IdPropiedad), write(' a sido hipotecada\n'), !.
juegoHipotecarPropiedad(Juego, _, Juego):- 
    write('No es posible realizar la hipoteca\n'), !.

% Descripción: Levanta la hipoteca de una propiedad
% Dominio: Juego (TDA juego), IdPropiedad (int), JuegoActualizado (TDA juego)
% Recorrido: JuegoActualizado
juegoLevantarHipoteca(Juego, IdPropiedad, JuegoActualizado):-
    juegoObtenerJugadorActual(Juego, Jugador),
    jugadorPoseePropiedad(Jugador, IdPropiedad),
    juegoObtenerPropiedad(Juego, IdPropiedad, Propiedad),
    propiedadEstaHipotecada(Propiedad),  % Debe estar hipotecada
    propiedadObtenerPrecio(Propiedad, Precio),
    juegoObtenerTasaImpuesto(Juego, Tasa),
    MontoBase is Precio // 2,
    Interes is (MontoBase * Tasa) // 100,
    MontoTotal is MontoBase + Interes,
    jugadorObtenerDinero(Jugador, DineroJugador),
    DineroJugador >= MontoTotal,  % Debe poder pagar
    propiedadDeshipotecar(Propiedad, PropiedadActualizada),
    juegoActualizarPropiedad(Juego, PropiedadActualizada, JuegoA1),
    juegoCobrarJugador(JuegoA1, Jugador, MontoTotal, JuegoActualizado),    
    write('La propiedad '), write(IdPropiedad), write(' a sido deshipotecada\n'), !.
juegoLevantarHipoteca(Juego, _, Juego):-
    write('No es posible levantar la hipoteca\n'), !.

% Descripcion: Calcula la renta de una propiedad
% Dominio: Juego (TDA juego) X propiedad (TDA propiedad) X Monto (number)
% Recorrido : integer
juegoCalcularRentaPropiedad(_, Propiedad, Monto):-% Caso : No es hotel
    \+ propiedadEsHotel(Propiedad),
    propiedadObtenerCasas(Propiedad, CantidadCasas),
    propiedadObtenerRenta(Propiedad, RentaBase),
    RentaCasas is RentaBase* 0.2 * CantidadCasas,
    Monto0 is RentaCasas + RentaBase,
    round(Monto0, Monto).
juegoCalcularRentaPropiedad(Juego, Propiedad, Monto):- %Caso : Es Hotel
    propiedadEsHotel(Propiedad),
    juegoObtenerMaximoCasas(Juego, MaximoCasas),
    propiedadObtenerRenta(Propiedad, RentaBase),
    MontoParcial is RentaBase * 0.2 * MaximoCasas,
    Monto0 is MontoParcial + RentaBase,
    round(Monto0, Monto).

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
    DineroJugador < Monto, 
    DineroJugadorActualizado is 0,
    DineroBancoActualizado is DineroBanco + DineroJugador,
    juegoSetDineroBanco(Juego, DineroBancoActualizado, JuegoBancoActualizado),
    jugadorSetDinero(Jugador, DineroJugadorActualizado, JugadorActualizado),
    juegoActualizarJugador(JuegoBancoActualizado, JugadorActualizado, JuegoActualizado).
juegoCobrarJugador(Juego, Jugador, Monto, JuegoActualizado):-
    juegoObtenerDineroBanco(Juego, DineroBanco),
    jugadorObtenerDinero(Jugador, DineroJugador),
    DineroJugador >= Monto, 
    DineroJugadorActualizado is DineroJugador - Monto,
    DineroBancoActualizado is DineroBanco + Monto,
    juegoSetDineroBanco(Juego, DineroBancoActualizado, JuegoBancoActualizado),
    jugadorSetDinero(Jugador, DineroJugadorActualizado, JugadorActualizado),
    juegoActualizarJugador(JuegoBancoActualizado, JugadorActualizado, JuegoActualizado).

% Descripcion: predicado para pagar renta cuando un player cae en propiedad de otro player
% Dominio: Juego (TDA juego) X Propiedad (TDA propiedad) X JuegoActualizado (TDA juego)
% Recorrido: juego
juegoPagarRenta(Juego, Propiedad ,JuegoActualizado):-
    propiedadObtenerDueno(Propiedad, Dueno),
    Dueno \= [],
    \+ propiedadEstaHipotecada(Propiedad), %propiedad hipotecada no cobra renta
    juegoObtenerJugadorActual(Juego, JugadorActual), jugadorObtenerId(JugadorActual, IDJugadorActual),
    IDJugadorActual \= Dueno,
    juegoObtenerJugador(Juego, Dueno, JugadorPropietario),
    juegoCalcularRentaPropiedad(Juego, Propiedad, Monto),
    jugadorPagarRenta(JugadorActual, JugadorPropietario, Monto, Jactual2, JProp2),
    juegoActualizarJugador(Juego, Jactual2, JuegoA1),
    juegoActualizarJugador(JuegoA1, JProp2, JuegoActualizado), 
    write('Renta pagada: '),writeln(Monto), !.
juegoPagarRenta(Juego, _, Juego):- !.

% Descripcion: Mueve al jugador en el tablero actualizando el juego, tambien corrige la posicion en caso de sobrepasar ultima posicion (y cobra impuestos).
% Dominio: Juego (TDA juego) X ID (integer) X Dados (lista integer) X JuegoActualizado (TDA juego)
% Recorrido: juego
juegoMoverJugador(Juego, ID, _, Juego) :-
    juegoObtenerJugador(Juego, ID, Jugador),
    jugadorEstaenCarcel(Jugador, true), !.
juegoMoverJugador(Juego, ID, Dados, JuegoActualizado):-
    juegoObtenerJugador(Juego, ID, Jugador),
    juegoObtenerUltimaPosicion(Juego, UltimaPosicion),
    jugadorObtenerPosicion(Jugador, Posicion),
    sumarLista(Dados, Movimiento),
    PosicionActualizada is Posicion + Movimiento,
    PosicionActualizada > UltimaPosicion,
    jugadorSetPosicion(Jugador, PosicionActualizada, JugadorPre), %Necesario cobrar impuesto antes de corregir posicion
    juegoActualizarJugador(Juego, JugadorPre, JuegoA1),
    juegoImpuestoPropiedades(JuegoA1, JuegoA2),
    juegoObtenerJugadorActual(JuegoA2, JugadorCobrado),
    jugadorCorregirPosicion(JugadorCobrado, UltimaPosicion, JugadorActualizado),
    juegoActualizarJugador(JuegoA2, JugadorActualizado, JuegoActualizado), !.
juegoMoverJugador(Juego, ID, Dados, JuegoActualizado):-
    juegoObtenerJugador(Juego, ID, Jugador),
    juegoObtenerUltimaPosicion(Juego, UltimaPosicion),
    jugadorObtenerPosicion(Jugador, Posicion),
    sumarLista(Dados, Movimiento),
    PosicionActualizada is Posicion + Movimiento,
    PosicionActualizada =< UltimaPosicion,  %en este caso no se cobran impuestos
    jugadorSetPosicion(Jugador, PosicionActualizada, JugadorPre),
    juegoActualizarJugador(Juego, JugadorPre, JuegoActualizado), !.


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
juegoImpuestoPropiedades(Juego, JuegoActualizado) :-
    juegoObtenerUltimaPosicion(Juego, UltimaPosicion),
    juegoObtenerJugadorActual(Juego, Jugador),
    jugadorObtenerPosicion(Jugador, PosicionJugador),
    PosicionJugador > UltimaPosicion,
    juegoCalcularRentaJugador(Juego, Jugador, Renta),
    juegoObtenerTasaImpuesto(Juego, TasaImpuesto),
    Cobro is (TasaImpuesto * Renta) // 100,
    juegoCobrarJugador(Juego, Jugador, Cobro, JuegoActualizado).
    
imprimirImpuestos(PosVerificar, UltimaPosicion):-
    PosVerificar > UltimaPosicion, writeln('Impuestos cobrados').
imprimirImpuestos(PosVerificar, UltimaPosicion):-
    PosVerificar =< UltimaPosicion.

% Descripcion: Obtiene una carta aleatoria de un tipo especificado
% Dominio: Juego (TDA juego) X TipoCarta (Atom) X Seed (integer) X NuevaSeed (integer) X Carta (TDA carta)
% Recorrido: JuegoActualizado X Carta
juegoExtraerCarta(Juego, TipoCarta, [Seed], [NuevaSeed], JuegoActualizado, Carta) :-
    juegoObtenerCartas(Juego, TipoCarta, Cartas),
    length(Cartas, CantidadCartas),
    getRandomInRange(Seed, NuevaSeed, CantidadCartas, Index),
    nth1(Index, Cartas, Carta),
    juegoEjecutarCarta(Juego, Carta, JuegoActualizado).

% Descripcion: Ejecuta la accion de una carta en un juego.
% Dominio: Juego (TDA juego) X Carta (TDA carta) X JuegoActualizado (TDA juego)
% Recorrido : juego
juegoEjecutarCarta(Juego, Carta, JuegoActualizado):-
    cartaObtenerAccion(Carta, AccionCarta),
    call(AccionCarta, Juego, JuegoActualizado).

% Descripcion: Paga multa para salir de la carcel (el jugador actual)
% Dominio: Juego (TDA juego) X JuegoActualizado (TDA juego)
% Recorrido : juego
juegoPagarMultaCarcel(Juego, _, JuegoActualizado):-
    juegoObtenerJugadorActual(Juego, Jugador),
    jugadorObtenerDinero(Jugador, Dinero),
    Dinero >= 500,
    jugadorSetEstaenCarcel(Jugador, false, JugadorA1),
    juegoCobrarJugador(Juego, JugadorA1, 500, JuegoActualizado), !.
juegoPagarMultaCarcel(Juego, _, Juego):- !.

% Descripción: Embarga al jugador si esta en bancarrota, quitándole todas sus propiedades
% Dominio: Juego(TDA juego) x JugadorId (integer) x JuegoActualizado(TDA juego)
% Recorrido: juego
juegoEmbargarJugador(Juego, IdJugador, Juego):-
    juegoObtenerJugador(Juego, IdJugador, Jugador),
    \+ jugadorEstaEnBancarrota(Jugador), !.
juegoEmbargarJugador(Juego, IdJugador, JuegoActualizado) :-
    juegoObtenerJugador(Juego, IdJugador, Jugador),
    jugadorEstaEnBancarrota(Jugador),
    jugadorObtenerPropiedades(Jugador, ListaProps),
    embargarPropiedades(Juego, ListaProps, JuegoSinProps),
    jugadorActualizarPropiedades(Jugador, [], JugadorEmbargado),
    juegoActualizarJugador(JuegoSinProps, JugadorEmbargado, JuegoActualizado),
    write('El jugador '), write(IdJugador), write(' ha sido embargado (bancarrota)\n').
% actualiza todas las propiedades quitandoles el dueño
% Recursion natural
embargarPropiedades(Juego, [], Juego).
embargarPropiedades(Juego, [IdProp | Resto], JuegoFinal) :-
    juegoObtenerPropiedad(Juego, IdProp, Prop),
    propiedadSetDueno(Prop, [], PropSinDuenio),
    juegoActualizarPropiedad(Juego, PropSinDuenio, JuegoParcial),
    embargarPropiedades(JuegoParcial, Resto, JuegoFinal).

% Descripcion: Avanza de turno el juego, finalizando el  juego si corresponde, embargando al jugador si queda en bancarrota.
% Dominio: Juego (TDA juego) X Juego (TDA juego)
% Recorrido: juego
juegoAvanzarTurno(Juego, JuegoFinalizado):-
    juegoObtenerTurnoActual(Juego, TurnoActual),
    juegoTerminar(Juego, JuegoA1), 
    juegoEmbargarJugador(JuegoA1, TurnoActual, JuegoFinalizado), !.

juegoAvanzarTurno(Juego, JuegoActualizado):-
    juegoObtenerTurnoActual(Juego,TurnoActual),
    juegoObtenerUltimoTurno(Juego, UltimoTurno),
    TurnoActual < UltimoTurno,
    TurnoActualizado is TurnoActual + 1,
    juegoEmbargarJugador(Juego, TurnoActual, JuegoA1),
    juegoSetTurnoActual(JuegoA1, TurnoActualizado, JuegoActualizado), !.

juegoAvanzarTurno(Juego, JuegoActualizado):-
    juegoObtenerTurnoActual(Juego,TurnoActual),
    juegoEmbargarJugador(Juego, TurnoActual, JuegoA1),
    juegoObtenerUltimoTurno(Juego, UltimoTurno),
    TurnoActual = UltimoTurno,
    juegoSetTurnoActual(JuegoA1, 1, JuegoActualizado).
   
% Descripcion: Predicado para ejecutar todo un turno. Separado en cuatro casos posibles.
% Dominio: Juego (TDA juego) X SeedDados (list integer) X NSeedDados (List integer) X Dados (list integer)
% Recorrido: juego
%Caso 0: Estaba encarcelado
juegoJugarTurno(Juego, SeedDados, NSeedDados, Accion, Argumento, JuegoActualizado):-
    juegoLanzarDados(Juego, SeedDados, NSeedDados, Dados),
    juegoObtenerJugadorActual(Juego, JugadorActual),
    jugadorEstaenCarcel(JugadorActual, Estado), Estado = true,
    write('Dados lanzados: '), writeln(Dados),
    call(Accion, Juego, Argumento, JuegoA2), %Ejecutar accion de input
    juegoAvanzarTurno(JuegoA2, JuegoActualizado), !.
%Caso 1: Cae en salida
juegoJugarTurno(Juego, SeedDados, NSeedDados, Accion, Argumento, JuegoActualizado):-
    juegoLanzarDados(Juego, SeedDados, NSeedDados, Dados), 
    juegoObtenerJugadorActual(Juego, JugadorActual), juegoObtenerUltimaPosicion(Juego, UltimaPosicion),
    sumarLista(Dados, MovTurno), jugadorObtenerPosicion(JugadorActual, PosActual), PosVerificar is PosActual + MovTurno,
    jugadorObtenerId(JugadorActual, ID),
    juegoMoverJugador(Juego, ID, Dados, JuegoA1), %juego con el pj movido
    juegoObtenerCasillaActual(JuegoA1, CasillaCaida),
    CasillaCaida = salida, %cae en la salida
    write('Dados lanzados: '), writeln(Dados),
    imprimirImpuestos(PosVerificar, UltimaPosicion),
    call(Accion, JuegoA1, Argumento, JuegoA2), %Ejecutar accion de input (como hipotecar alguna propiedad)
    juegoAvanzarTurno(JuegoA2, JuegoActualizado), !.

%Caso 2: Cae en la carcel
juegoJugarTurno(Juego, SeedDados, NSeedDados, _, _,JuegoActualizado):-
    juegoLanzarDados(Juego, SeedDados, NSeedDados, Dados),
    juegoObtenerJugadorActual(Juego, JugadorActual), juegoObtenerUltimaPosicion(Juego, UltimaPosicion),
    sumarLista(Dados, MovTurno), jugadorObtenerPosicion(JugadorActual, PosActual), PosVerificar is PosActual + MovTurno,
    jugadorObtenerId(JugadorActual, ID),
    juegoMoverJugador(Juego, ID, Dados, JuegoA1), %juego con el pj movido
    juegoObtenerCasillaActual(JuegoA1, CasillaCaida),
    CasillaCaida = carcel,
    write('Dados lanzados: '), writeln(Dados),
    imprimirImpuestos(PosVerificar, UltimaPosicion),
    writeln('Ha caido en la carcel'),
    irACarcel(JuegoA1, JuegoA2), %encarcelado y pierde su accion de turno
    juegoAvanzarTurno(JuegoA2, JuegoActualizado), !.

%Caso 3: Cae en una carta
juegoJugarTurno(Juego, [S1|TS], NSeedDados, Accion, Argumento, JuegoActualizado):-
    juegoLanzarDados(Juego, [S1|TS], NSeedDados, Dados),
    juegoObtenerJugadorActual(Juego, JugadorActual),  juegoObtenerUltimaPosicion(Juego, UltimaPosicion),
    sumarLista(Dados, MovTurno), jugadorObtenerPosicion(JugadorActual, PosActual), PosVerificar is PosActual + MovTurno,
    jugadorObtenerId(JugadorActual, ID),
    juegoMoverJugador(Juego, ID, Dados, JuegoA1), %juego con el pj movido
    juegoObtenerCasillaActual(JuegoA1, CasillaCaida),
    (CasillaCaida = suerte ; CasillaCaida = comunidad), %cae en carta
    write('Dados lanzados: '), writeln(Dados),
    imprimirImpuestos(PosVerificar, UltimaPosicion),
    juegoExtraerCarta(JuegoA1, CasillaCaida, [S1], _, JuegoA2, Carta), %ejecutar carta
    cartaObtenerDescripcion(Carta, Descripcion),
    write('Carta extraida: '), writeln(Descripcion),
    call(Accion, JuegoA2, Argumento, JuegoA3), %Accion de turno (como hipotecar una prop)
    juegoAvanzarTurno(JuegoA3, JuegoActualizado), !. 

%Caso 4: Cae en una propiedad
juegoJugarTurno(Juego, SeedDados, NSeedDados, Accion, Argumento, JuegoActualizado):-
    juegoLanzarDados(Juego, SeedDados, NSeedDados, Dados),
    juegoObtenerJugadorActual(Juego, JugadorActual),  juegoObtenerUltimaPosicion(Juego, UltimaPosicion),
    sumarLista(Dados, MovTurno), jugadorObtenerPosicion(JugadorActual, PosActual), PosVerificar is PosActual + MovTurno,
    jugadorObtenerId(JugadorActual, ID),
    juegoMoverJugador(Juego, ID, Dados, JuegoA1), %juego con el pj movido
    juegoObtenerCasillaActual(JuegoA1, CasillaCaida),
    esPropiedad(CasillaCaida),
    write('Dados lanzados: '), writeln(Dados),
    imprimirImpuestos(PosVerificar, UltimaPosicion),
    juegoPagarRenta(JuegoA1, CasillaCaida, JuegoA2), %pagar renta si corresponde
    call(Accion, JuegoA2, Argumento, JuegoA3), %accion de turno (posible compra de propiedad)
    juegoAvanzarTurno(JuegoA3, JuegoActualizado), !.

juegoJugarTurno(Juego, _, _, _,_, Juego):- write('\n-fallo al jugar turno-\n'). % para depurar errores


% Descripcion: Comienza el juego (El turno deja de ser 0)
% Dominio :Juego (TDA Juego)
% Recorrido : juego
juegoComenzar(Juego, JuegoComienza):-
    juegoObtenerTurnoActual(Juego, TurnoActual), TurnoActual = 0,
    juegoSetTurnoActual(Juego, 1, JuegoComienza), 
    write('\n--- El Juego comienza ---\n\n'), !.
juegoComenzar(Juego, Juego):- write('-Error no es posible comenzar-'), !.

% Descripcion: finaliza el juego si solo queda un jugador no en bancarrota
% Dominio: Juego (TDA juego) X JuegoActualizado(TDA juego)
% Recorrido: juego
juegoTerminar(Juego, JuegoActualizado) :-
    juegoObtenerJugadores(Juego, Jugadores),
    hayUnGanador(Jugadores, Ganador),
    \+ jugadorEstaEnBancarrota(Ganador),
    write('----- Juego terminado -----\n'),
    juegoSetTurnoActual(Juego, -1, JuegoActualizado).

% Retorna ganador si solo hay un jugador no en bancarrota
hayUnGanador(Jugadores, Ganador) :-
    excluirBancarrota(Jugadores, NoBancarrota),
    NoBancarrota = [Ganador], !.
%Excluye jugadores en bancarrota con el fin de llegar al ganador
excluirBancarrota([], []).
excluirBancarrota([J|Resto], [J|Filtrados]) :-
    \+ jugadorEstaEnBancarrota(J),
    excluirBancarrota(Resto, Filtrados).
excluirBancarrota([J|Resto], Filtrados) :-
    jugadorEstaEnBancarrota(J),
    excluirBancarrota(Resto, Filtrados).

% (esta no cuenta como accion carta)
% Descripcion: Usa una carta para salir de la carcel el jugador actual
% Dominio: Juego (TDA juego)  X JuegoActualizado (TDA juego)
% Recorrido : juego
cartaSalirCarcel(Juego, _, Juego):-
    juegoObtenerJugadorActual(Juego, JugadorActual),
    jugadorEstaenCarcel(JugadorActual, Estado),
    jugadorObtenerCartasCarcel(JugadorActual, CartasCarcel),
    (CartasCarcel = 0 ; Estado = false), !.
cartaSalirCarcel(Juego, _, JuegoActualizado):-
    juegoObtenerJugadorActual(Juego, JugadorActual),
    jugadorUsarCartaCarcel(JugadorActual, JugadorActualizado),
    juegoActualizarJugador(Juego, JugadorActualizado, JuegoActualizado),
    write('Has usado una carta para ser liberado\n'), !.

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

% Descripcion: Le da una carta para salir de la carcel al jugador actual
% Dominio: Juego (TDA juego)  X JuegoActualizado (TDA juego)
% Recorrido : juego
obtenerCartaSalirCarcel(Juego, JuegoActualizado):-
    juegoObtenerJugadorActual(Juego, JugadorActual),
    jugadorObtenerCartasCarcel(JugadorActual, CartasCarcel),
    CartasCarcelAct is CartasCarcel + 1,
    jugadorSetCartasCarcel(JugadorActual, CartasCarcelAct, JugadorActualizado),
    juegoActualizarJugador(Juego, JugadorActualizado, JuegoActualizado).

% Descripcion: Agrega 5000 de dinero al jugador actual
% Dominio: Juego (TDA juego)  X JuegoActualizado (TDA juego)
% Recorrido : juego
ganarKino(Juego, JuegoActualizado):-
    juegoObtenerJugadorActual(Juego, Jugador),
    juegoPagarJugador(Juego, Jugador, 5000, JuegoActualizado).

% Duplica el dinero del jugador
% Dominio: Juego (TDA juego)  X JuegoActualizado (TDA juego)
% Recorrido : juego
duplicaDinero(Juego, JuegoActualizado):-
    juegoObtenerJugadorActual(Juego, Jugador),
    jugadorObtenerDinero(Jugador, Dinero),
    juegoPagarJugador(Juego, Jugador, Dinero, JuegoActualizado).

% Descripcion: Paga el 400 de su dinero al banco (multa sin mas)
% Dominio: Juego (TDA juego)  X JuegoActualizado (TDA juego)
% Recorrido : juego
multaSuerte(Juego, JuegoActualizado):-
    juegoObtenerJugadorActual(Juego, Jugador),
    juegoCobrarJugador(Juego, Jugador, 400, JuegoActualizado).

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
    TasaActual =< 10, TasaActualizada is 15,
    juegoSetTasaImpuesto(Juego, TasaActualizada, JuegoActualizado).
cambiarImpuesto(Juego, JuegoActualizado):-
    juegoObtenerTasaImpuesto(Juego, TasaActual),
    TasaActual >= 15, TasaActualizada is 5,
    juegoSetTasaImpuesto(Juego, TasaActualizada, JuegoActualizado).
cambiarImpuesto(Juego, JuegoActualizado):-
    juegoObtenerTasaImpuesto(Juego, TasaActual),
    TasaActual =< 5, TasaActualizada is 10,
    juegoSetTasaImpuesto(Juego, TasaActualizada, JuegoActualizado).    

% Descripcion: Libera a todos los jugadores de la carcel
% Dominio: Juego (TDA juego)  X JuegoActualizado (TDA juego)
% Recorrido : juego
liberarPrisioneros(Juego, JuegoActualizado):-
    juegoObtenerJugadores(Juego, Jugadores),
    liberarPresosAux(Jugadores, JugadoresActualizados),
    juegoActualizarJugadores(Juego, JugadoresActualizados, JuegoActualizado).
liberarPresosAux([], []).
liberarPresosAux([JActual|JResto], [JALibre | RestoLibre]):-
    jugadorSetEstaenCarcel(JActual, false, JALibre),
    liberarPresosAux(JResto, RestoLibre).

% Descripcion: Otorga un bono de dinero
% Dominio: Juego (TDA juego)  X JuegoActualizado (TDA juego)
% Recorrido : juego
bonoDinero(Juego, JuegoActualizado):-
    juegoObtenerJugadorActual(Juego, JugadorActual),
    juegoPagarJugador(Juego, JugadorActual, 800, JuegoActualizado).

% Descripcion: impuestos del 40% o del 1% (cambia como un switch)
% Dominio: Juego (TDA juego)  X JuegoActualizado (TDA juego)
% Recorrido : juego
crisisEconomica(Juego, JuegoActualizado):-
    juegoObtenerTasaImpuesto(Juego, TasaActual),
    TasaActual =< 10, TasaActualizada is 40,
    juegoSetTasaImpuesto(Juego, TasaActualizada, JuegoActualizado).
crisisEconomica(Juego, JuegoActualizado):-
    juegoObtenerTasaImpuesto(Juego, TasaActual),
    TasaActual > 10, TasaActualizada is 1,
    juegoSetTasaImpuesto(Juego, TasaActualizada, JuegoActualizado).

% Descripcion: Bono de 400 para todos
% Dominio: Juego (TDA juego)  X JuegoActualizado (TDA juego)
% Recorrido : juego
bonoEquitativo(Juego, JuegoActualizado) :-
    juegoObtenerJugadores(Juego, Jugadores),
    bonificarJugadores(Jugadores, Juego, JuegoActualizado).
bonificarJugadores([], Juego, Juego). %caso base
bonificarJugadores([Jugador | Resto], JuegoIn, JuegoOut) :- %caso recursivo
    juegoPagarJugador(JuegoIn, Jugador, 400, JuegoMedio),
    bonificarJugadores(Resto, JuegoMedio, JuegoOut).