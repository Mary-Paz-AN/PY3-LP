% Crgar la base de conocimiento %
:- consult('BC.pl').

% Guarda los nuevos hechos agregados al archivo %
guardarHechos :- 
    tell('BC.pl'),          % Abre el archivo y redirige la salida a este archivo
    listing(destino/2),      % Escribe todos los hechos del predicado destino/2
    listing(actividad/5),    % Escribe todos los hechos del predicado actividad/5
    listing(asociar_actividad/2), % Escribe todos los hechos de asociar_actividad/2
    told.                    % Cierra el archivo y vuelve a la salida normal.
 

% Pregunta por la información del destino %
agregarDestino :- 
    write('Nombre del destino: '),
    read(Nombre), nl,

    % Pregunta por la descripción y la convierto en una cadena de texto %
    write('Escriba una brve descripion del destino: '), 
    read_line_to_string(user_input, Descripcion), nl,

    % Guarda el destino en el archivo y en la base de conocimiento %
    assertz(destino(Nombre, Descripcion)),
    guardarHechos,
    write('Destino registrado exitosamente.'), nl, nl.

% Pregunta por la informacion de actividad %
agregarActividad :-
    % Pregunta por el nombre de la actividad %
    write('Nombre de la actividad: '),
    read(Nombre), nl,

    % Pregunta por el costo y verifica que sea un entero %
    write('Ingrese el costo de la actividad por persona: '),
    read(Costo), nl,
    (integer(Costo) -> 
        % Si el costo es un entero, preguntar por los días
        write('Ingrese la duración en días de la actividad: '),
        read(Dias), nl,
        (integer(Dias) ->
            % Pregunta por la descripción y la convierte en una cadena de texto %
            write('Escriba una breve descripción de la actividad: '), 
            read_line_to_string(user_input, Descripcion), nl,

            % Guarda la actividad en la base de conocimiento
            write('Actividad registrada exitosamente.'), nl, nl
        ;
            % Si los días no son un entero, mostrar mensaje de error
            write('Por favor ingrese un número entero válido para los días.'), nl, nl,
            agregarActividad  % Volver a preguntar
        )
    ;
        % Si el costo no es un entero, mostrar mensaje de error
        write('Por favor ingrese un número entero válido para el costo.'), nl, nl,
        agregarActividad  % Volver a preguntar
    ).

% Preguntapor la información para asociar la actividad (Falta verificar que el destino exista y tambien la actividad)%
agregarAsociarActividad :- 
    write('Nombre del destino: '),
    read(Destino), nl,

    % Verifica si el destino existe
    (destino(Destino, _) -> 
        write('Nombre de la actividad: '),
        read(Actividad), nl,

        % Verifica si la actividad existe
        (actividad(Actividad, _, _, _, _) ->  
            assertz(asociar_actividad(Destino, Actividad)),
            guardarHechos,
            write('Actividad asociada correctamente.'), nl, nl
        ;
            write('La actividad no existe.'), nl, nl,
            agregarAsociarActividad  
        )
    ;
        write('El destino no existe.'), nl, nl,
        agregarAsociarActividad 
    ).

% Menú para agregar hechos %
menuAgregarHechos :- 
    write('a. Destino'), nl,
    write('b. Actividad'), nl,
    write('c. Asociar Actividad'), nl,
    write('d. Volver'), nl,
    write('Ingrese la opcion que desea: '),
    read(Opcion), nl, nl,
    manejarOpcionHechos(Opcion).

% Maneja las opciones de agregar hechos %
manejarOpcionHechos(a) :- agregarDestino, menuAgregarHechos.
manejarOpcionHechos(b) :- agregarActividad, menuAgregarHechos.
manejarOpcionHechos(c) :- agregarAsociarActividad, menuAgregarHechos.
manejarOpcionHechos(d) :- !, true.
manejarOpcionHechos(_) :- write('Opción inválida, vuelva a intentarlo.'), nl, nl, menuAgregarHechos.


% Consultar destino %
consultarDestino :- write("Consultar destino"), nl.

% Actividades por tipo %
actividadXtipo :-
    write("Ingrese el tipo de actividad: "),
    read(Tipo),
    actividades_por_tipo(Tipo, Resultados),
    mostrar_resultados(Resultados).

mostrar_resultados([]) :-
    write("No se encontraron actividades para este tipo."), nl.
mostrar_resultados([(Destino, Nombre, Costo, Duracion, Descripcion)|T]) :-
    write("Destino: "), write(Destino), nl,
    write("Actividad: "), write(Nombre), nl,
    write("Costo: "), write(Costo), nl,
    write("Duraci�n: "), write(Duracion), nl,
    write("Descripci�n: "), write(Descripcion), nl, nl,
    mostrar_resultados(T).

% Genenarar itinerario por monto %
itinerarioXmonto :- write("Genenarar itinerario por monto"), nl.

% Consulta por precio
consultaXprecio :-
    write("Ingrese el monto: "),
    read(Monto),
    write("Desea consultar actividades (m)�s baratas o (c)aras? "),
    read(Opcion),
    (   Opcion = m -> actividades_mas_baratas(Monto, Resultados)
    ;   Opcion = c -> actividades_mas_caras(Monto, Resultados)
    ;   write("Opci�n inv�lida."), nl, consultaXprecio
    ),
    mostrar_resultados(Resultados).

% Predicados para encontrar actividades por precio
actividades_mas_baratas(Monto, Resultados) :-
    findall((Destino, Nombre, Costo, Duracion, Descripcion),
            (actividad(Nombre, Costo, Duracion, Descripcion, _),
             Costo =< Monto,
             asociar_actividad(Destino, Nombre)),
            Resultados).

actividades_mas_caras(Monto, Resultados) :-
    findall((Destino, Nombre, Costo, Duracion, Descripcion),
            (actividad(Nombre, Costo, Duracion, Descripcion, _),
             Costo > Monto,
             asociar_actividad(Destino, Nombre)),
            Resultados).

 % Genarar itinerario por dias %
itinerarioXdias :- write("Genarar itinerario por dias"), nl.

% Recomendar por frase %
recomendarXfrase :- write("Recomendar por frase"), nl.

% Estadisticas %
estadisticas :- write("Estadisticas"), nl.

actividades_por_tipo(Tipo, Resultados) :-
    findall((Destino, Nombre, Costo, Duracion, Descripcion),
            (actividad(Nombre, Costo, Duracion, Descripcion, Tipos),
             member(Tipo, Tipos),
             asociar_actividad(Destino, Nombre)),
            Resultados).

% Menú %
menu :-
    write('a. Agregar hechos'), nl,
    write('b. Consulta de destino'), nl,
    write('c. Actividades por tipo'), nl,
    write('d. Consulta por precio'), nl,
    write('e. Generar itinerario por monto'), nl,
    write('f. Generar itinerario por días'), nl,
    write('g. Recomendar por frase'), nl,
    write('h. Estadísticas'), nl,
    write('j. Volver'), nl,
    write('Ingrese la opcion que desea: '),
    read(Opcion), nl, nl,
    manejarOpMenu(Opcion).

% Maneja qué se hace en cada opción con corte para evitar falsos.
manejarOpMenu(a) :- menuAgregarHechos, menu.
manejarOpMenu(b) :- consultarDestino, menu.
manejarOpMenu(c) :- actividadXtipo, menu.
manejarOpMenu(d) :- consultaXprecio, menu.
manejarOpMenu(e) :- itinerarioXmonto, menu.
manejarOpMenu(f) :- itinerarioXdias, menu.
manejarOpMenu(g) :- recomendarXfrase, menu.
manejarOpMenu(h) :- estadisticas, menu.
manejarOpMenu(j) :- !, true.
manejarOpMenu(_) :- write('Opción inválida, vuelva a intentarlo.'), nl,nl, menu.


% Menú Principal %
menuPrincipal :-
    write('m. Menú'), nl,
    write('s. Salir'), nl,
    write('Ingrese la opcion que desea: '),
    read(Opcion), nl, nl,
    manejarOpciones(Opcion).

% Maneja qué se hace en cada opción con corte para evitar falsos.
manejarOpciones(m) :- menu, menuPrincipal.
manejarOpciones(s) :- write('Saliendo del programa...'), nl, true.
manejarOpciones(_) :- write('Opción inválida, vuelva a intentarlo.'), nl, nl, menuPrincipal.

% iniciar el programa %
iniciar :- menuPrincipal.
