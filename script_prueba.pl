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

    % comprobar la ultima posicion en el tablero
    tableroObtenerUltimaPosicion(T4, UltimaPos),
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
    juegoAgregarTablero(G2, T4, G3),
    write('Jugadores y tablero agregados: '), writeln(G3),

    %Obtener jugador de turno
    juegoObtenerJugadorActual(G3, JugadorActual),
    write('Jugador turno actual: '), writeln(JugadorActual),

    %probar lanzar dados
    juegoLanzarDados(G3, [3, 4], _, Dados),
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

    % actualizar propiedad con dueño
    juegoActualizarPropiedad(G4, Propiedad2_1, G5),

    % intentar pagar renta (No le alcanza, paga todo lo q tiene)
    writeln('Intentar pagar renta de 500...(J2 paga a J1)'),
    jugadorPagarRenta(J2_1, J1_0, 500, J2_2, J1_1),
    write('Jugadores actualizados: '), write(J2_2), write(" | "), writeln(J1_1),

    % intentar pagar renta (Pagar normalmente)
    writeln('Intentar pagar renta de 1300...(J1 paga a J2)'),
    jugadorPagarRenta(J1_1, J2_2, 1300, J1_2, J2_3),
    write('Jugadores actualizados: '), write(J1_2), write(" | "), writeln(J2_3),

    % actualizar jugadores tras compra de propiedad y prueba de pago de rentas
    juegoActualizarJugadores(G5, [J1_2, J2_3], G6),
    
    % Obtener posicion (propiedad despues de ser comprada)
    jugadorObtenerPosicion(J2_3, Pos2),
    juegoObtenerPosicion(G6, Pos2, Prop2_2),

    % construir casa
    juegoConstruirCasa(G6, Prop2_2, G7),
    write('Casa construida: '), writeln(G7),

    % propiedad con casa construida
    juegoObtenerPosicion(G7, Pos2, Prop2_3),

    % renta con una casa
    juegoCalcularRentaPropiedad(G7, Prop2_3, Renta),
    write('Renta propiedad 2 : '), writeln(Renta),
    juegoExtraerCarta(G7, suerte, [1], _, G8, Carta0),
    write('Carta extraida: '), writeln(Carta0),
    write('Carta Aplicada al juego: \n'), writeln(G8).



:- initialization(prueba).