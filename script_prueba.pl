% Cargar los archivos

:- use_module([tda_jugador, tda_propiedad, tda_tablero, tda_carta, tda_juego]).

% --- TESTING ---

prueba :-
    writeln('-------- Prueba 1: Creacion de Jugador y Getters --------'),
    % Crear un jugador
    jugador(1, 'Ana', 1500, [], 0, false, 0, J1),
    write('Jugador creado: '), writeln(J1),
    (esJugador(J1) -> writeln('Es un jugador valido.') ; writeln('No es un jugador valido')),

    jugador(2, 'Tonino', 1500, [], 0, false, 0, J2),
    write('Jugador creado: '), writeln(J2),
    (esJugador(J2) -> writeln('Es un jugador valido.') ; writeln('No es un jugador valido')),

    % Probar getter
    jugadorObtenerNombre(J1, NombreAna),
    write('Nombre de player 1: '), writeln(NombreAna),
    jugadorObtenerId(J1, ID1),
    write('ID de player 1: '), writeln(ID1),
    jugadorObtenerPropiedades(J1, Props1),
    write('Propiedades de player 1: '), writeln(Props1),

    % Crear propiedades
    writeln('-------- Prueba 2: Creacion de propiedades y compra --------'),
    propiedad(1, 'Avenida Principal', 2000, 200, [], 0, false, false, Propiedad1),
    write('Propiedad 1 creada: '), writeln(Propiedad1),

    propiedad(2, 'Calle Secundaria', 1200, 100, [], 0, false, false, Propiedad2),
    write('Propiedad 2 creada: '), writeln(Propiedad2),

    % intentar comprar propiedad
    writeln('Intentar comprar propiedad 2...'),
    jugadorComprarPropiedad(J1, Propiedad2, J1_1),
    propiedadSetDueno(Propiedad2, ID1, Propiedad2_1),
    write('Jugador actualizado: '), writeln(J1_1),
    write('Propiedad actualizada: '), writeln(Propiedad2_1),
    (esJugador(J1_1) -> writeln('Es un jugador valido.') ; writeln('No es un jugador valido')),
    (esPropiedad(Propiedad2_1) -> writeln('Es una propiedad valida.') ; writeln('No es una propiedad valida')),

    % intentar pagar renta (No le alcanza, paga todo lo q tiene)
    writeln('Intentar pagar renta de 500...(J1 paga a J2)'),
    jugadorPagarRenta(J1_1, J2, 500, J1_2, J2_1),
    write('Jugadores actualizados: '), write(J1_2), write(" | "), writeln(J2_1),

    % intentar pagar renta (Pagar normalmente)
    writeln('Intentar pagar renta de 700...(J2 paga a J1)'),
    jugadorPagarRenta(J2_1, J1_2, 700, J2_2, J1_3),
    write('Jugadores actualizados: '), write(J1_3), write(" | "), writeln(J2_2). 

:- initialization(prueba).