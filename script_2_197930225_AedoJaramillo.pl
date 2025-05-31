:- use_module(main_197930225_AedoJaramillo).

prueba_mario :-  
    writeln('\n-------- CAPITALIA: MARIO EDITION (SCRIPT DE PRUEBA)--------\n'),

    % Crear jugadores
    jugador(1, 'Mario', 0, [], 0, false, 1, J1), 
    write('Jugador creado: '), writeln(J1),

    jugador(2, 'Luigi', 0, [], 0, false, 1, J2),
    write('Jugador creado: '), writeln(J2),

    jugador(3, 'Peach', 0, [], 0, false, 1, J3),
    write('Jugador creado: '), writeln(J3),

    % Crear propiedades
    
    propiedad(1, 'Plaza Yoshi', 500, 120, [], 0, false, false, P1),
    propiedad(2, 'Llanuras Donut', 600, 150, [], 0, false, false, P2),
    propiedad(3, 'Isla Delfino', 900, 225, [], 0, false, false, P3),
    propiedad(4, 'Mansion Boo', 1000, 260, [], 0, false, false, P4),
    propiedad(5, 'Bosque ilusion', 1200, 350, [], 0, false, false, P5),
    propiedad(6, 'Fortaleza koopa', 1800, 500, [], 0, false, false, P6),
    propiedad(7, 'Puente de queso', 600, 150, [], 0, false, false, P7),
    propiedad(8, 'Monte Galleta', 700, 180, [], 0, false, false, P8),
    propiedad(9, 'Cupula Vainilla', 800, 200, [], 0, false, false, P9),
    propiedad(10, 'Fortaleza de Whomp', 950, 240, [], 0, false, false, P10),
    propiedad(11, 'Lago de soda', 1100, 300, [], 0, false, false, P11),
    propiedad(12, 'Isla Chocolate', 1400, 400, [], 0, false, false, P12),
    propiedad(13, 'Camino Estelar', 1500, 420, [], 0, false, false, P13),
    propiedad(14, 'Valle de Bowser', 1600, 450, [], 0, false, false, P14),
    propiedad(15, 'Castillo de Bowser', 2000, 600, [], 0, false, false, P15),

    write('Propiedades creadas.\n'),

    % Crear cartas
    carta(1, suerte, 'Una tuberia te lleva a la salida.', irASalida, C1),
    carta(2, suerte, 'Has sido recluido en el calabozo.', irACarcel, C2),
    carta(3, suerte, 'Has ganado la loteria! (Banco paga 5.000).', ganarKino, C3),
    carta(4, suerte, 'Obtiene carta para salir del calabozo', obtenerCartaSalirCarcel, C4),
    carta(5, suerte, 'Paga multa de 400', multaSuerte, C5),
    carta(6, comunidad, 'Bowser cambia la tasa de impuestos del Reino', cambiarImpuesto, C6),
    carta(7, comunidad, 'Paga el 10% de tus monedas en impuestos del Reino', pagarImpuesto, C7),
    carta(8, comunidad, 'Kamek ha liberado a todos los prisioneros!', liberarPrisioneros, C8),
    write('Cartas creadas.\n'),

    % Crear tablero
    tablero([] ,[], [], [], T0),
    write('Tablero vacio: '), writeln(T0),

    % Crear tablero y agregarle
    tableroAgregarPropiedades(T0, [[P1, 1], [P2, 2],[P3, 4],[P4, 6],
        [P5, 8],[P6, 10],[P7, 11],[P8, 13],[P9, 14],[P10, 16],
        [P11, 17],[P12, 18],[P13, 20],[P14, 21],[P15, 23]], T1),

    % Agregar casillas especiales al tablero
    tableroAgregarCasillasEspeciales(T1, 
        [[salida, 0],
        [suerte, 3],
        [comunidad, 5],
        [suerte, 7],
        [comunidad, 9],
        [carcel, 12],
        [suerte, 15],
        [suerte, 19],
        [comunidad, 22]], T2),

    tableroAgregarCartas(T2, [C1,C2,C3,C4,C5], [C6,C7,C8], T3),
    write('Tablero completado: \n'), writeln(T3),

    tableroObtenerUltimaPosicion(T3, UltimaPos),
    write('Ultima posicion en el tablero: '), writeln(UltimaPos),

    % Crear juego
    juego([], [], 35000, 3, 1, 10, 9, 1, G0),
    juegoAgregarJugador(G0, J1, G1),
    juegoAgregarJugador(G1, J2, G2),
    juegoAgregarJugador(G2, J3, G3),

    juegoAgregarTablero(G3, T3, G4),

    write('Juego listo para comenzar: \n'), writeln(G4), write('\n'),

    % Obtener jugador de turno
    juegoObtenerJugadorActual(G4, JugadorActual),
    write('Jugador de turno actual: '), writeln(JugadorActual), write('\n'),

    % Semillas para dados
    Seeds0 = [932, 162, 82],
    
    % Jugar turno 1
    juegoJugarTurno(G4, Seeds0, Seeds1, juegoAccionComprarPropiedad, _, G5),
    write('Juego tras turno 1 (Mario): \n'), writeln(G5), write('\n'),

    % Jugar turno 2
    juegoJugarTurno(G5, Seeds1, Seeds2, juegoAccionComprarPropiedad, _, G6),
    write('Juego tras turno 2 (Luigi): \n'), writeln(G6), write('\n'),

    % Jugar turno 3
    juegoJugarTurno(G6, Seeds2, Seeds3, juegoAccionComprarPropiedad, _, G7),
    write('Juego tras turno 3 (Peach): \n'), writeln(G7), write('\n'),

    % Jugar turno 4
    juegoJugarTurno(G7, Seeds3, Seeds4, juegoAccionComprarPropiedad, _, G8),
    write('Juego tras turno 4(Mario): \n'), writeln(G8), write('\n'),

    % Jugar turno 5
    juegoJugarTurno(G8, Seeds4, Seeds5, juegoAccionComprarPropiedad, _, G9),
    write('Juego tras turno 5(Luigi): \n'), writeln(G9), write('\n'),

    % Jugar turno 6
    juegoJugarTurno(G9, Seeds5, Seeds6, juegoAccionComprarPropiedad, _, G10),
    write('Juego tras turno 6 (Peach): \n'), writeln(G10), write('\n'),

    % Jugar turno 7
    juegoJugarTurno(G10, Seeds6, Seeds7, juegoAccionComprarPropiedad, _, G11),
    write('Juego tras turno 7(Mario): \n'), writeln(G11), write('\n'),

    % Jugar turno 8
    juegoJugarTurno(G11, Seeds7, Seeds8, juegoAccionComprarPropiedad, _, G12),
    write('Juego tras turno 8(Luigi): \n'), writeln(G12), write('\n'),

    % Jugar turno 9
    juegoJugarTurno(G12, Seeds8, Seeds9, juegoAccionComprarPropiedad, _, G13),
    write('Juego tras turno 9 (Peach): \n'), writeln(G13), write('\n'),
    
    % Jugar turno 10
    juegoJugarTurno(G13, Seeds9, Seeds10, juegoAccionComprarPropiedad, _, G14),
    write('Juego tras turno 10(Mario): \n'), writeln(G14), write('\n'),

    % Jugar turno 11
    juegoJugarTurno(G14, Seeds10, Seeds11, juegoConstruirCasa, 11, G15),
    write('Juego tras turno 11(Luigi): \n'), writeln(G15), write('\n'),

    % Jugar turno 12
    juegoJugarTurno(G15, Seeds11, Seeds12, juegoAccionComprarPropiedad, _, G16),
    write('Juego tras turno 12 (Peach): \n'), writeln(G16), write('\n'),
    
    % Jugar turno 13
    juegoJugarTurno(G16, Seeds12, Seeds13, juegoAccionComprarPropiedad, _, G17),
    write('Juego tras turno 13(Mario): \n'), writeln(G17), write('\n'),

    % Jugar turno 14
    juegoJugarTurno(G17, Seeds13, Seeds14, juegoAccionComprarPropiedad, _, G18),
    write('Juego tras turno 14(Luigi): \n'), writeln(G18), write('\n'),

    % Jugar turno 15
    juegoJugarTurno(G18, Seeds14, Seeds15, juegoAccionComprarPropiedad, _, G19),
    write('Juego tras turno 15 (Peach): \n'), writeln(G19), write('\n'),
    
    % Jugar turno 16
    juegoJugarTurno(G19, Seeds15, Seeds16, juegoAccionComprarPropiedad, _, G20),
    write('Juego tras turno 16(Mario): \n'), writeln(G20), write('\n'),

    % Jugar turno 17
    juegoJugarTurno(G20, Seeds16, Seeds17, cartaSalirCarcel, _, G21),
    write('Juego tras turno 17(Luigi): \n'), writeln(G21), write('\n'),

    % Jugar turno 18
    juegoJugarTurno(G21, Seeds17, Seeds18, juegoAccionComprarPropiedad, _, G22),
    write('Juego tras turno 18 (Peach): \n'), writeln(G22), write('\n'),
    
    % Jugar turno 19
    juegoJugarTurno(G22, Seeds18, Seeds19, juegoAccionComprarPropiedad, _, G23),
    write('Juego tras turno 19(Mario): \n'), writeln(G23), write('\n'),

    % Jugar turno 20
    juegoJugarTurno(G23, Seeds19, Seeds20, cartaSalirCarcel, _, G24),
    write('Juego tras turno 20(Luigi): \n'), writeln(G24), write('\n'),

    % Jugar turno 21
    juegoJugarTurno(G24, Seeds20, Seeds21, juegoAccionComprarPropiedad, _, G25),
    write('Juego tras turno 21 (Peach): \n'), writeln(G25), write('\n'),
    
    % Jugar turno 22
    juegoJugarTurno(G25, Seeds21, Seeds22, juegoAccionComprarPropiedad, _, G26),
    write('Juego tras turno 22(Mario): \n'), writeln(G26), write('\n'),

    % Jugar turno 23
    juegoJugarTurno(G26, Seeds22, Seeds23, juegoAccionComprarPropiedad, _, G27),
    write('Juego tras turno 23(Luigi): \n'), writeln(G27), write('\n'),

    % Jugar turno 24
    juegoJugarTurno(G27, Seeds23, Seeds24, cartaSalirCarcel, _, G28),
    write('Juego tras turno 24 (Peach): \n'), writeln(G28), write('\n'),
    
    % Jugar turno 25
    juegoJugarTurno(G28, Seeds24, _, juegoAccionComprarPropiedad, _, G29),
    write('Juego tras turno 25(Mario): \n'), writeln(G29), write('\n').

:- initialization(prueba_mario).