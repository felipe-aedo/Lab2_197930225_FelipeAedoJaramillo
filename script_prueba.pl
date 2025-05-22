% Cargar los archivos

:- use_module([tda_jugador, tda_propiedad, tda_tablero, tda_carta, tda_juego]).

% --- TESTING ---

prueba :-
    writeln('\n-------- Prueba 1: Creacion de Jugador y Getters --------\n'),
    % Crear jugadores
    jugador(1, 'Meli', 0, [], 0, false, 0, J1),
    write('Jugador creado: '), writeln(J1),
    (esJugador(J1) -> writeln('Es un jugador valido.') ; writeln('No es un jugador valido')),

    jugador(2, 'Tonino', 0, [], 0, false, 0, J2),
    write('Jugador creado: '), writeln(J2),
    (esJugador(J2) -> writeln('Es un jugador valido.') ; writeln('No es un jugador valido')),

    % Crear propiedades
    writeln('\n-------- Prueba 2: Creacion de propiedades y tablero --------\n'),
    propiedad(1, 'Avenida Principal', 2000, 200, [], 0, false, false, Propiedad1),
    write('Propiedad 1 creada: '), writeln(Propiedad1),
    (esPropiedad(Propiedad1) -> writeln('Es una propiedad valida.') ; writeln('No es una propiedad valida')),

    propiedad(2, 'Calle Secundaria', 1200, 100, [], 0, false, false, Propiedad2),
    write('Propiedad 2 creada: '), writeln(Propiedad2),
    (esPropiedad(Propiedad2) -> writeln('Es una propiedad valida.') ; writeln('No es una propiedad valida')),

    propiedad(3, 'Costanera norte', 1700, 145, [], 0, false, false, Propiedad3),
    write('Propiedad 3 creada: '), writeln(Propiedad3),
    (esPropiedad(Propiedad3) -> writeln('Es una propiedad valida.') ; writeln('No es una propiedad valida')),

    propiedad(4, 'Costanera sur', 1500, 135, [], 0, false, false, Propiedad4),
    write('Propiedad 4 creada: '), writeln(Propiedad4),
    (esPropiedad(Propiedad4) -> writeln('Es una propiedad valida.') ; writeln('No es una propiedad valida')),

    propiedad(5, 'Pio Nono', 1400, 125, [], 0, false, false, Propiedad5),
    write('Propiedad 5 creada: '), writeln(Propiedad5),
    (esPropiedad(Propiedad5) -> writeln('Es una propiedad valida.') ; writeln('No es una propiedad valida')),

    % Crear tablero
    tablero([] ,[], [], [], T1),
    write('Tablero vacio: '), writeln(T1),

    % Agregar componentes al tablero
    tableroAgregarPropiedades(T1, [[Propiedad1, 1], [Propiedad2, 2], [Propiedad3, 3], [Propiedad4, 4], [Propiedad5, 6]], T2),
    tableroAgregarCasillasEspeciales(T2, [[salida, 0],[carcel, 5]], T3),
    write('Tablero con propiedades y casillas especiales: '), writeln(T3),

    % comprobar la ultima posicion en el tablero
    tableroObtenerUltimaPosicion(T3, UltimaPos),
    write('Ultima posicion en el tablero: '), writeln(UltimaPos), 

    %intentar crear un juego
    writeln('\n-------- Prueba 3: Juego y prueba de predicados --------\n'),
    juego([], [], 25000, 2, 2, 10, 8, 1, G0), %En el turno 2 para probar el getJugadorActual
    write('Juego creado: '), writeln(G0),
    (esJuego(G0) -> writeln('Es un juego valido.') ; writeln('No es un juego valido')),

    %Agregar jugadores
    juegoAgregarJugador(G0, J1, G1),
    juegoAgregarJugador(G1, J2, G2),

    %Agregar tablero
    juegoAgregarTablero(G2, T3, G3),
    write('Jugadores y tablero agregados: '), writeln(G3),

    %Obtener jugador de turno
    juegoObtenerJugadorActual(G3, JugadorActual),
    write('Jugador turno actual: '), writeln(JugadorActual),

    %probar lanzar dados
    juegoLanzarDados(G3, [5, 4], _, Dados),
    write('Dados lanzados: '), writeln(Dados),

    %intentar mover jugador
    juegoMoverJugador(G3, 2, Dados, G4),
    write('Juego actualizado (Tonino se mueve completando una vuelta): '), writeln(G4),

    % intentar comprar propiedad
    writeln('\n-------- Prueba 4: Probar transacciones --------\n'),
    juegoObtenerJugadores(G4, [J2_0, J1_0]),

    writeln('Intentar comprar propiedad 2...'),
    jugadorComprarPropiedad(J2_0, Propiedad2, J2_1),
    jugadorObtenerId(J2_0, ID1),
    propiedadSetDueno(Propiedad2, ID1, Propiedad2_1),
    write('Jugador actualizado: '), writeln(J2_1),
    write('Propiedad actualizada: '), writeln(Propiedad2_1),
    (esJugador(J2_1) -> writeln('Es un jugador valido.') ; writeln('No es un jugador valido')),
    (esPropiedad(Propiedad2_1) -> writeln('Es una propiedad valida.') ; writeln('No es una propiedad valida')),

    % intentar pagar renta (No le alcanza, paga todo lo q tiene)
    writeln('Intentar pagar renta de 500...(J2 paga a J1)'),
    jugadorPagarRenta(J2_1, J1_0, 500, J2_2, J1_1),
    write('Jugadores actualizados: '), write(J2_2), write(" | "), writeln(J1_1),

    % intentar pagar renta (Pagar normalmente)
    writeln('Intentar pagar renta de 700...(J1 paga a J2)'),
    jugadorPagarRenta(J1_1, J2_2, 700, J1_2, J2_3),
    write('Jugadores actualizados: '), write(J1_2), write(" | "), writeln(J2_3).


:- initialization(prueba).