% Cargar los archivos
:- use_module(tda_jugador).

% --- TESTING ---

prueba1 :-
    writeln('--- Prueba 1: Creacion de Jugador y Getters ---'),
    % Crear un jugador
    jugador(1, 'Ana', 1500, [], 0, false, 0, Jugador1),
    write('Jugador creado: '), writeln(Jugador1),
    (es_jugador(Jugador1) -> writeln('Es un jugador valido.') ; writeln('No es un jugador valido')),
    % Probar getter
    jugador_obtener_nombre(Jugador1, NombreAna),
    write('Nombre de player 1: '), writeln(NombreAna),
    jugador_obtener_id(Jugador1, ID1),
    write('ID de player 1: '), writeln(ID1),
    jugador_obtener_propiedades(Jugador1, Props1),
    write('Propiedades de player 1: '), writeln(Props1),
    writeln('Agregar propiedad 3...'),
    jugador_agregar_propiedad(Jugador1, 3, Jugador1_1),
    write('Jugador actualizado: '), writeln(Jugador1_1),
    (es_jugador(Jugador1_1) -> writeln('Es un jugador valido.') ; writeln('No es un jugador valido')).

:- initialization(prueba1).