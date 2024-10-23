% Crgar la base de conocimiento %
:- consult('BC.pl').

% Agregar hechos %
agregarHechos :- write("Agregar hechos"), nl.

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
manejarOpMenu(a) :- agregarHechos, menu.
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
