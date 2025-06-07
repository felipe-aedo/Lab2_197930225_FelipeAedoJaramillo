:- use_module(main_197930225_AedoJaramillo).

prueba_base:-
    writeln('\n-------- CAPITALIA (SCRIPT DE PRUEBA BASE)--------\n'),
    % Crear jugadores
    jugador( 1, 'jugador1', 0, [], 0, false, 0, J1),
    jugador( 2, 'jugador2', 0, [], 0, false, 0, J2),
    write('Jugadores creados: \n'), writeln(J1), writeln(J2),

    propiedad(21,  'Avenida Bar HbH', 1200, 300, [], 0, false, false, P1),
    propiedad(22,  'Universidad de Santiago', 1100, 250, [], 0, false, false, P2),
    propiedad(23,  'Estacion Central', 800, 150, [], 0, false, false, P3),
    propiedad(24,  'Cerrillos', 900, 200, [], 0, false, false, P4),
    propiedad(25,  'Universidad de Chile', 1090, 250, [], 0, false, false, P5),
    propiedad(26,  'Plaza de Maipu', 1000, 235, [], 0, false, false, P6),

    carta( 1, "suerte", "Avance hasta la casilla de salida", irASalida, C1),
    carta( 2, "suerte", "Mover el jugador a la carcel", irACarcel, C2),
    carta( 3, "comunidad", "Cambia el impuesto del juego", cambiarImpuesto, C3),
    carta( 4, "suerte", "Gana 5.000", ganarKino, C4),

    tablero([], [], [], [], T1),
    juego([], T1, 200000, 2, 0, 10, 4, 1, G1),
    tableroAgregarPropiedades(T1, [[P1, 2], [P2, 3], [P3, 6], [P4, 8], [P5, 10], [P6, 12]], T1_v1),
    tableroAgregarCartas(T1_v1, [C1,C2,C3], [C4], T1_v12),
    tableroAgregarCasillasEspeciales(T1_v12, [[salida, 0],[suerte, 1],[suerte, 4], [carcel, 5], [comunidad, 7], [suerte, 9], [comunidad, 11]] ,T1_v2),
    write('\nTablero para este juego: \n'), write(T1_v2),
    juegoAgregarJugador( G1, J1, G1_v2),
    juegoAgregarJugador( G1_v2, J2, G1_v3),
    juegoAgregarTablero(G1_v3, T1_v2, G1_v41),
    juegoComenzar(G1_v41, G1_v4),

    write('\nJuego listo para comenzar: \n'), writeln(G1_v4),

    juegoObtenerJugadorActual(G1_v4, JA_v1), % JA_v1 debería ser igual a J1
    write('\nJugador turno actual: '), writeln(JA_v1),

    SDado1 = 1, SDado2 = 2, 
    juegoLanzarDados(G1_v4, [SDado1, SDado2], [SDado1_v2, SDado2_v2], DADOS),
    write('Dados lanzados: '), writeln(DADOS),

    jugadorObtenerId(JA_v1, ID_JA_v1), 
    juegoMoverJugador(G1_v4, ID_JA_v1, DADOS, G1_v5),
    juegoComprarPropiedad(G1_v5, G1_v6),
    juegoObtenerJugadorActual(G1_v6, J1_v2), %PLAYER CON LA PROPIEDAD COMPRADA
    juegoObtenerPropiedad(G1_v6, 22, Prop22Comprada),
    juegoCalcularRentaPropiedad(G1_v6, Prop22Comprada, MONTO_RENTA_P1),

    write('Renta de la propiedad comprada: '), writeln(MONTO_RENTA_P1),
    juegoCalcularRentaJugador(G1_v6, J1_v2, MONTO_RENTA_JACTUAL_v3), %imprimir rentas
    write('Renta del jugador 1: '), write(MONTO_RENTA_JACTUAL_v3),
    writeln('\nJuego tras turno 1:'), writeln(G1_v6),

    writeln('\n\nSiguiente turno'), %avanzar turno
    juegoAvanzarTurno(G1_v6, G1_v7),

    juegoObtenerJugadorActual(G1_v7, JA_v2), % JA_v1 debería ser igual a J2
    write('Jugador turno actual: '), writeln(JA_v2),
    juegoJugarTurno(G1_v7, [SDado1_v2, SDado2_v2], Seeds3, juegoAccionComprarPropiedad, _, G1_v8),
    writeln('Juego tras turno 2: '), writeln(G1_v8), write('\n'), %cae en carta suerte

    juegoObtenerJugadorActual(G1_v8, JA_v3), % JA_v1 debería ser igual a J1
    write('Jugador turno actual: '), writeln(JA_v3),
    juegoJugarTurno(G1_v8, Seeds3, Seeds4, juegoConstruirCasa, 22, G1_v9), %construye una casa
    writeln('Juego tras turno 3: '), writeln(G1_v9), write('\n'), %cae en carta suerte

    juegoObtenerJugadorActual(G1_v9, JA_v4), % JA_v1 debería ser igual a J2
    write('Jugador turno actual: '), writeln(JA_v4),
    juegoJugarTurno(G1_v9, Seeds4, Seeds5, juegoAccionComprarPropiedad, _, G1_v10), %no puede comprar
    writeln('Juego tras turno 4: '), writeln(G1_v10), write('\n'), %cae en carta suerte

    juegoObtenerJugadorActual(G1_v10, JA_v5), % JA_v1 debería ser igual a J1
    write('Jugador turno actual: '), writeln(JA_v5),
    juegoJugarTurno(G1_v10, Seeds5, Seeds6, juegoHipotecarPropiedad, 22, G1_v11),
    writeln('Juego tras turno 5: '), writeln(G1_v11), write('\n'),%cae en carta suerte

    juegoObtenerJugadorActual(G1_v11, JA_v6), % JA_v1 debería ser igual a J2
    write('Jugador turno actual: '), writeln(JA_v6),
    juegoJugarTurno(G1_v11, Seeds6, _, juegoAccionComprarPropiedad, _, G1_v12),
    writeln('Juego tras turno 6: '), writeln(G1_v12), write('\n'), %cae en carta suerte

    juegoObtenerJugadorActual(G1_v12, JA_v7), % JA_v1 debería ser igual a J1
    write('Jugador turno actual: '), writeln(JA_v7),
    juegoJugarTurno(G1_v12, [1,3], _, juegoConstruirCasa, 22, G1_v13), %construye una segunda casa
    writeln('Juego tras turno 7: '), writeln(G1_v13), write('\n'),
    
    juegoObtenerJugadorActual(G1_v13, JA_v8), % JA_v1 debería ser igual a J2
    write('Jugador turno actual: '), writeln(JA_v8),
    juegoJugarTurno(G1_v13, [2,3], _, juegoAccionComprarPropiedad, _, G1_v14),
    writeln('Juego tras turno 8: '), writeln(G1_v14), write('\n'), %Compra plaza de maipu

    juegoObtenerJugadorActual(G1_v14, JA_v9), % JA_v1 debería ser igual a J1
    write('Jugador turno actual: '), writeln(JA_v9),
    juegoJugarTurno(G1_v14, [4,1], _, juegoAccionComprarPropiedad, _, G1_v15),
    writeln('Juego tras turno 9: '), writeln(G1_v15).

:- initialization(prueba_base).