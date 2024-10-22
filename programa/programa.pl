% Crgar la base de conocimiento %
:- consult('BC.pl').

% Agregar hechos %
agregarHechos :- write("Agregar hechos"), nl.

% Consultar destino %
consultarDestino :- write("Consultar destino"), nl.

% Actividades por tipo %
actividadXtipo :- write("Actividades por tipo"), nl.

% Consulta por pecio %
consultaXprecio :- write("Consulta por pecio"), nl.

% Genenarar itinerario por monto %
itinerarioXmonto :- write("Genenarar itinerario por monto"), nl.

% Genarar itinerario por dias %
itinerarioXdias :- write("Genarar itinerario por dias"), nl.

% Recomendar por frase %
recomendarXfrase :- write("Recomendar por frase"), nl.

% Estadisticas %
estadisticas :- write("Estadisticas"), nl.


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
    read(Opcion), nl,
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
manejarOpMenu(j) :- !, menuPrincipal.
manejarOpMenu(_) :- write('Opción inválida, vuelva a intentarlo.'), nl, menu.


% Menú Principal %
menuPrincipal :- 
    write('m. Menú'), nl,
    write('s. Salir'), nl,
    write('Ingrese la opcion que desea: '),
    read(Opcion), nl,
    manejarOpciones(Opcion).

% Maneja qué se hace en cada opción con corte para evitar falsos.
manejarOpciones(m) :- menu, menuPrincipal.
manejarOpciones(s) :- write('Saliendo del programa...'), nl. 
manejarOpciones(_) :- write('Opción inválida, vuelva a intentarlo.'), nl, menuPrincipal.

% iniciar el programa %
iniciar :- menuPrincipal.