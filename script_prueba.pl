% Cargar los archivos

:- use_module([tda_jugador, tda_propiedad, tda_tablero, tda_carta, tda_juego]).

% --- TESTING ---

prueba :-
    writeln('\n-------- CAPITALIA (SCRIPT DE PRUEBA)--------\n'),
    % Crear jugadores
    jugador(1, 'Meli', 0, [], 0, false, 0, J1),
    write('Jugador creado: '), writeln(J1),

    jugador(2, 'Tonino', 0, [], 0, false, 0, J2),
    write('Jugador creado: '), writeln(J2),

    % Crear propiedades
    propiedad(1, 'Avenida Principal', 2000, 200, [], 0, false, false, Propiedad1),
    write('Propiedad 1 creada: '), writeln(Propiedad1),

    propiedad(2, 'Calle Secundaria', 1200, 100, [], 0, false, false, Propiedad2),
    write('Propiedad 2 creada: '), writeln(Propiedad2),

    propiedad(3, 'Costanera norte', 1700, 145, [], 0, false, false, Propiedad3),
    write('Propiedad 3 creada: '), writeln(Propiedad3),

    propiedad(4, 'Costanera sur', 1500, 135, [], 0, false, false, Propiedad4),
    write('Propiedad 4 creada: '), writeln(Propiedad4),

    propiedad(5, 'Pio Nono', 1400, 125, [], 0, false, false, Propiedad5),
    write('Propiedad 5 creada: '), writeln(Propiedad5),

    % Crear cartas
    carta( 1, suerte, 'Avance hasta la casilla de salida', irASalida, C1),
    carta( 2, suerte, 'Ha sido encarcelado', irACarcel, C2),
    carta( 3, suerte, 'Gana 20.000', ganarKino, C3),
    carta( 4, comunidad, 'Cambia el impuesto del juego', cambiarImpuesto, C4),
    carta( 5, comunidad, 'Pague el 10% de su dinero al banco', pagarImpuesto, C5),

    % Crear tablero
    tablero([] ,[], [], [], T1),
    write('Tablero vacio: '), writeln(T1),

    % Agregar componentes al tablero
    tableroAgregarPropiedades(T1, [[Propiedad1, 1], [Propiedad2, 2], [Propiedad3, 4], [Propiedad4, 6], [Propiedad5, 8]], T2),
    tableroAgregarCasillasEspeciales(T2, [[salida, 0],[suerte, 3],[carcel, 5], [comunidad, 7]], T3),
    tableroAgregarCartas(T3, [C1,C2,C3], [C4,C5], T4),
    write('Tablero con todos los componentes: \n'), writeln(T4),

    % Comprobar la ultima posicion en el tablero
    tableroObtenerUltimaPosicion(T4, UltimaPos),
    write('Ultima posicion en el tablero: '), writeln(UltimaPos), 

    % Crear un juego
    juego([], [], 25000, 2, 2, 10, 8, 1, G0), %En el turno 2 para probar el getJugadorActual
    write('Juego creado: '), writeln(G0),
    (esJuego(G0) -> writeln('Es un juego valido.') ; writeln('No es un juego valido')),

    % Agregar jugadores
    juegoAgregarJugador(G0, J1, G1),
    juegoAgregarJugador(G1, J2, G2),

    % Agregar tablero
    juegoAgregarTablero(G2, T4, G3),
    write('Jugadores y tablero agregados: '), writeln(G3),

    % Obtener jugador de turno
    juegoObtenerJugadorActual(G3, JugadorActual),
    write('Jugador turno actual: '), writeln(JugadorActual), write('\n'),

    Seeds0 = [3,4],
    % Jugar turno 1
    juegoJugarTurno(G3, Seeds0, Seeds1, juegoAccionComprarPropiedad, _, G4),
    write('Juego tras turno 1 (Tonino): \n'), writeln(G4), write('\n'),

    % Jugar turno 2
    juegoJugarTurno(G4, Seeds1, Seeds2, juegoAccionComprarPropiedad, _, G5),
    write('Juego tras turno 2 (Meli): \n'), writeln(G5), write('\n'),


    % Jugar turno 3
    juegoJugarTurno(G5, Seeds2, Seeds3, juegoAccionComprarPropiedad, _, G6),
    write('Juego tras turno 3 (Tonino): \n'), writeln(G6), write('\n'),


    % Jugar turno 4
    juegoJugarTurno(G6, Seeds3, Seeds4, juegoAccionComprarPropiedad, _, G7),
    write('Juego tras turno 4(Meli): \n'), writeln(G7), write('\n'),


    % Jugar turno 5
    juegoJugarTurno(G7, Seeds4, _, juegoAccionComprarPropiedad, _, G8),
    write('Juego tras turno 5(Tonino): \n'), writeln(G8).

    



:- initialization(prueba).