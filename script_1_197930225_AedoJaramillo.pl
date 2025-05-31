% Cargar los archivos

:- use_module(main_197930225_AedoJaramillo).

% --- TESTING ---

prueba :-
    writeln('\n-------- CAPITALIA (SCRIPT DE PRUEBA)--------\n'),
    % Crear jugadores
    %  dinero = 0 / posicion = 0 / carrcel = false / 1 carta para salir de carcel

    jugador(1, 'Tonino', 0, [], 0, false, 1, J1), 
    write('Jugador creado: '), writeln(J1),

    jugador(2, 'Meli', 0, [], 0, false, 1, J2),
    write('Jugador creado: '), writeln(J2),

    % Crear propiedades
    propiedad(1, 'Avenida Principal', 2000, 200, [], 0, false, false, Propiedad1),
    write('Propiedad 1 creada: '), writeln(Propiedad1),

    propiedad(2, 'Calle Secundaria', 1200, 100, [], 0, false, false, Propiedad2),
    write('Propiedad 2 creada: '), writeln(Propiedad2),

    propiedad(3, 'Costanera norte', 1700, 145, [], 0, false, false, Propiedad3),
    write('Propiedad 3 creada: '), writeln(Propiedad3),

    propiedad(4, 'Costanera sur', 1470, 135, [], 0, false, false, Propiedad4),
    write('Propiedad 4 creada: '), writeln(Propiedad4),

    propiedad(5, 'Pio Nono', 1300, 120, [], 0, false, false, Propiedad5),
    write('Propiedad 5 creada: '), writeln(Propiedad5),

    % Crear cartas
    carta( 1, suerte, 'Avance hasta la casilla de salida', irASalida, C1),
    carta( 2, suerte, 'Ha sido encarcelado', irACarcel, C2),
    carta( 3, suerte, 'Ha ganado la loteria! (banco paga $5.000)', ganarKino, C3),
    carta( 4, suerte, 'Obtiene carta para salir de la carcel', obtenerCartaSalirCarcel, C4),
    carta( 5, comunidad, 'Cambia el impuesto del juego', cambiarImpuesto, C5),
    carta( 6, comunidad, 'Pague el 10% de su dinero al banco', pagarImpuesto, C6),
    carta( 7, comunidad, 'Todos los prisioneros son liberados', liberarPrisioneros, C7),

    % Crear tablero
    tablero([] ,[], [], [], T1),
    write('Tablero vacio: '), writeln(T1),

    % Agregar componentes al tablero
    tableroAgregarPropiedades(T1, [[Propiedad1, 1], [Propiedad2, 2], [Propiedad3, 4], [Propiedad4, 6], [Propiedad5, 8]], T2),
    tableroAgregarCasillasEspeciales(T2, [[salida, 0],[suerte, 3],[carcel, 5], [comunidad, 7]], T3),
    tableroAgregarCartas(T3, [C1,C2,C3,C4], [C5,C6,C7], T4),
    write('Tablero con todos los componentes: \n'), writeln(T4),

    % Comprobar la ultima posicion en el tablero
    tableroObtenerUltimaPosicion(T4, UltimaPos),
    write('Ultima posicion en el tablero: '), writeln(UltimaPos), 

    % Crear un juego
    juego([], [], 25000, 2, 2, 10, 8, 1, G0), 
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
    write('Juego tras turno 1 (Meli): \n'), writeln(G4), write('\n'),

    % Jugar turno 2
    juegoJugarTurno(G4, Seeds1, Seeds2, juegoAccionComprarPropiedad, _, G5),
    write('Juego tras turno 2 (Tonino): \n'), writeln(G5), write('\n'),

    % Jugar turno 3
    juegoJugarTurno(G5, Seeds2, Seeds3, juegoAccionComprarPropiedad, _, G6),
    write('Juego tras turno 3 (Meli): \n'), writeln(G6), write('\n'),

    % Jugar turno 4
    juegoJugarTurno(G6, Seeds3, Seeds4, juegoAccionComprarPropiedad, _, G7),
    write('Juego tras turno 4(Tonino): \n'), writeln(G7), write('\n'),

    % Jugar turno 5
    juegoJugarTurno(G7, Seeds4, Seeds5, juegoAccionComprarPropiedad, _, G8),
    write('Juego tras turno 5(Meli): \n'), writeln(G8), write('\n'),

    % Jugar turno 6
    juegoJugarTurno(G8, Seeds5, Seeds6, juegoAccionComprarPropiedad, _, G9),
    write('Juego tras turno 6(Tonino): \n'), writeln(G9), write('\n'),

    % Jugar turno 7 (tonino usa su carta carcel)
    juegoJugarTurno(G9, Seeds6, Seeds7, cartaSalirCarcel, _, G10),
    write('Juego tras turno 7(Meli): \n'), writeln(G10), write('\n'),
    
    % Jugar turno 8
    juegoJugarTurno(G10, Seeds7, Seeds8, cartaSalirCarcel, _, G11),
    write('Juego tras turno 8(Tonino): \n'), writeln(G11), write('\n'),

    % Jugar turno 9
    juegoJugarTurno(G11, Seeds8, Seeds9, juegoHipotecarPropiedad, 2, G12),
    write('Juego tras turno 9(Meli): \n'), writeln(G12), write('\n'),
    
    % Jugar turno 10
    juegoJugarTurno(G12, Seeds9, Seeds10, juegoAccionComprarPropiedad, _, G13),
    write('Juego tras turno 10(Tonino): \n'), writeln(G13), write('\n'),

    % Jugar turno 11
    juegoJugarTurno(G13, Seeds10, _, juegoLevantarHipoteca, 2, G14),
    write('Juego tras turno 11(Meli): \n'), writeln(G14), write('\n'),
    
    % Juego turno 12
    juegoJugarTurno(G14, [1,2], _, juegoAccionComprarPropiedad, _, G15),
    write('Juego tras turno 12(Tonino): \n'), writeln(G15), write('\n').
    

:- initialization(prueba).