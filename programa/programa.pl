% Crgar la base de conocimiento %
:- consult('BC.pl').


% Guarda los hechos de destino al archivo %
guardarDestino(Nombre, Descripcion) :- 
    assertz(destino(Nombre, Descripcion)),

    % Abrir el archivo en modo de agregar sin borrar el contenido
    open('BC.pl', append, Stream),
    
    % Escribir el hecho en el archivo
    write(Stream, 'destino('),
    write(Stream, Nombre),
    write(Stream, ', \''),
    write(Stream, Descripcion),
    write(Stream, '\''),  
    write(Stream, ').'), 
    nl(Stream), 
    close(Stream). % Cerrar el archivo

% Escribir en el archivo el las categorias en la lista
guardarTipos(Stream, [Ultimo]) :-
    write(Stream, '\''),
    write(Stream, Ultimo),
    write(Stream, '\'').

guardarTipos(Stream, [Cabeza | Resto]) :-
    write(Stream, '\''),
    write(Stream, Cabeza),
    write(Stream, '\''),
    write(Stream, ', '),
    guardarTipos(Stream, Resto).

% Guarda los hechos de actividad al archivo %
guardarActividad(Nombre, Costo, Duracion, Descripcion, Tipos) :- 
    assertz(actividad(Nombre, Costo, Duracion, Descripcion, Tipos)),

    % Abrir el archivo en modo de agregar sin borrar el contenido
    open('BC.pl', append, Stream),
    
    % Escribir el hecho en el archivo
    write(Stream, 'actividad('),
    write(Stream, Nombre),
    write(Stream, ', '),
    write(Stream, Costo),
    write(Stream, ', '),
    write(Stream, Duracion),
    write(Stream, ', \''),
    write(Stream, Descripcion),
    write(Stream, '\''),
    write(Stream, ', ['),
    guardarTipos(Stream, Tipos),
    write(Stream, ']).'), 
    nl(Stream),    
    close(Stream).

% Guarda los hechos de asociar_actividad al archivo %
guardarAsociarActividad(Destino, Actividad) :- 
    assertz(asociar_actividad(Destino, Actividad)),

    % Abrir el archivo en modo de agregar sin borrar el contenido
    open('BC.pl', append, Stream),
    
    % Escribir el hecho en el archivo
    write(Stream, 'asociar_actividad('),
    write(Stream, Destino),
    write(Stream, ', '),
    write(Stream, Actividad),
    write(Stream, ').'), 
    nl(Stream), 
    close(Stream). % Cerrar el archivo


% Verifica que el destino exista en la base de conocimiento
verificarDestino(Destino) :-
    destino(Destino, _).

% Verifica que la actividad exista en la base de conocimiento
verificarActividad(Actividad) :-
    actividad(Actividad, _, _, _, _).

% Pregunta por la información del destino %
agregarDestino :- 
    write('Ingrese el nombre del destino: '),
    read(Nombre), nl,
    (   verificarDestino(Nombre) -> 
            write('El destino ya existe. Porfavor agregue otro.'), nl,
            agregarDestino
    ;   write('Escriba una breve descripción del destino: '), 
            read(DescripcionAtom), nl,

            % Verifica que lo que se escribio es un string
            atom_string(DescripcionAtom, Descripcion),
            guardarDestino(Nombre, Descripcion),
            write('Destino registrado exitosamente.'), nl, nl
    ).

% Pregunta por el costo y verifica si es un entero
preguntarXCostoActividad(Costo) :-
    write('Ingrese el costo de la actividad: '),
    read(CostoInput), nl,
    
    % Verificar si es entero
    (   integer(CostoInput) -> Costo is CostoInput
    ;   write('El costo debe ser un entero.'), nl,
        preguntarXCostoActividad(Costo)
    ).

% Pregunta por la verificar y verifica si es un entero
preguntarXDuracionActividad(Duracion) :-
    write('Ingrese el duración de la actividad (dias): '),
    read(DuracionInput), nl,
    
    % Verificar si es entero
    (   integer(DuracionInput) -> Duracion is DuracionInput
    ;   write('La duración debe ser un entero.'), nl,
        preguntarXDuracionActividad(Duracion)
    ).

% Agrega categoria a la lista
agregarCategoria(Elemento, Lista, [Elemento | Lista]).

% Pregunta por la categoria que quiere agregar
preguntarXTipoActividad(ListaTipo, ListaTipoFinal) :-
    write('1. Arte'), nl,
    write('2. Cultura'), nl,
    write('3. Historia'), nl,
    write('4. Arquitectura'), nl,
    write('5. Panorama'), nl,
    write('6. Diversión'), nl,
    write('7. Romántico'), nl,
    write('8. Gastronomía'), nl,
    write('9. Naturaleza'), nl,
    write('10. Educativo'), nl,
    write('11. Experiencia'), nl,
    write('12. Aventura'), nl,
    write('0. Dejar de añadir'), nl,
    write('Ingrese la categoria de la actividad: '),
    read(Cat),
    (   Cat = 1 -> 
            agregarCategoria('arte', ListaTipo, NuevaListaTipo),
            preguntarXTipoActividad(NuevaListaTipo, ListaTipoFinal)
    ;   Cat = 2 -> 
            agregarCategoria('cultura', ListaTipo, NuevaListaTipo),
            preguntarXTipoActividad(NuevaListaTipo, ListaTipoFinal)
    ;   Cat = 3 -> 
            agregarCategoria('historia', ListaTipo, NuevaListaTipo),
            preguntarXTipoActividad(NuevaListaTipo, ListaTipoFinal)
    ;   Cat = 4 -> 
            agregarCategoria('arquitectura', ListaTipo, NuevaListaTipo),
            preguntarXTipoActividad(NuevaListaTipo, ListaTipoFinal)
    ;   Cat = 5 -> 
            agregarCategoria('panorama', ListaTipo, NuevaListaTipo),
            preguntarXTipoActividad(NuevaListaTipo, ListaTipoFinal)
    ;   Cat = 6 -> 
            agregarCategoria('diversion', ListaTipo, NuevaListaTipo),
            preguntarXTipoActividad(NuevaListaTipo, ListaTipoFinal)
    ;   Cat = 7 -> 
            agregarCategoria('romantico', ListaTipo, NuevaListaTipo),
            preguntarXTipoActividad(NuevaListaTipo, ListaTipoFinal)
    ;   Cat = 8 -> 
            agregarCategoria('gastronomia', ListaTipo, NuevaListaTipo),
            preguntarXTipoActividad(NuevaListaTipo, ListaTipoFinal)
    ;   Cat = 9 -> 
            agregarCategoria('naturaleza', ListaTipo, NuevaListaTipo),
            preguntarXTipoActividad(NuevaListaTipo, ListaTipoFinal)
    ;   Cat = 10 -> 
            agregarCategoria('educacion', ListaTipo, NuevaListaTipo),
            preguntarXTipoActividad(NuevaListaTipo, ListaTipoFinal)
    ;   Cat = 11 -> 
            agregarCategoria('experiencia', ListaTipo, NuevaListaTipo),
            preguntarXTipoActividad(NuevaListaTipo, ListaTipoFinal)
    ;   Cat = 12 -> 
            agregarCategoria('aventura', ListaTipo, NuevaListaTipo),
            preguntarXTipoActividad(NuevaListaTipo, ListaTipoFinal)
    ;   Cat = 0 ->
            (   ListaTipo \= [] -> 
                ListaTipoFinal = ListaTipo,
                write('Categorías añadidas exitosamente.'), nl
            ;   write('Agregue al menos una categoría antes de salir.'), nl,
                preguntarXTipoActividad(ListaTipo, ListaTipoFinal)
            )
    ;   write('Opción inválida. Vuelva a intentarlo.'), nl,
        preguntarXTipoActividad(ListaTipo, ListaTipoFinal)
    ).
    
% Pregunta por la informacion de actividad %
agregarActividad :-
    write('Ingrese el nombre de la actividad: '),
    read(Nombre), nl,
    (   verificarActividad(Nombre) ->
            write('La actividad ya existe. Porfavor, agregue otra.'), nl,
            agregarActividad
    ;   preguntarXCostoActividad(Costo),
            preguntarXDuracionActividad(Duracion),

            write('Escriba una breve descripcion de la actividad: '),
            read(DescripAtm), nl,
            atom_string(DescripAtm, Descripcion),

            preguntarXTipoActividad([], ListaTipoFinal),

            guardarActividad(Nombre, Costo, Duracion, Descripcion, ListaTipoFinal),
            write('Destino registrado exitosamente.'), nl, nl
    ).

% Pregunta por la actividad y verificar que exista
preguntarActividadAA(Actividad) :-
    write('Ingrese el nombre de la actividad: '),
    read(ActividadInput), nl,

    (   verificarActividad(ActividadInput) ->
            Actividad = ActividadInput
    ;   write('La actividad no existe. Porfavor ingrese una existente.'), nl,
            preguntarActividadAA(Actividad)
    ).

% Preguntapor la información para asociar la actividad 
agregarAsociarActividad :- 
    write('Ingrese el nombre del destino: '),
    read(Destino), nl,

    % Verificar que el destino exista
    (   verificarDestino(Destino) ->
            preguntarActividadAA(Actividad),
            guardarAsociarActividad(Destino, Actividad),
            write('Actividad asociada exitosamente.'), nl, nl
    ;   write('El destino no existe. Porfavor ingrese uno existente.'), nl,
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


% Imprime las categorías de una actividad
mostrarTipos([Ultimo]) :-
    write(Ultimo).

mostrarTipos([Cabeza | Resto]) :-
    write(Cabeza),
    write(', '),
    mostrarTipos(Resto).

% Imprime en pantalla las actividades dadas y acumula el costo y la duración
mostrarActividades([], TiempoTotal, CostoTotal, TiempoTotal, CostoTotal). 

mostrarActividades([Cabeza | Resto], TiempoAcum, CostoAcum, TiempoTotal, CostoTotal) :-
    actividad(Cabeza, Costo, Duracion, Descripcion, Tipos), 
    write('Nombre: '), write(Cabeza),
    nl, write('Costo: '), write(Costo),
    nl, write('Duración: '), write(Duracion), write(' dias'),
    nl, write('Descripción: '), write(Descripcion),
    nl, write('Categorias: '), mostrarTipos(Tipos), nl, nl,
    
    % Actualizar los acumuladores
    NuevoTiempoAcum is TiempoAcum + Duracion,
    NuevoCostoAcum is CostoAcum + Costo,
    
    % Llamada recursiva con acumuladores actualizados
    mostrarActividades(Resto, NuevoTiempoAcum, NuevoCostoAcum, TiempoTotal, CostoTotal).


% Consultar destino
consultarDestino :- 
    write("Ingrese el destino que desea consultar: "), 
    read(Destino), nl,

    % Encuentra todas las actividades asociadas al destino
    findall(
        Actividad,
        asociar_actividad(Destino, Actividad),
        Actividades),
    
    % Verificar que existan actividades asociadas
    (   Actividades \= [] ->
            mostrarActividades(Actividades, 0, 0, TiempoTotal, CostoTotal),
            nl, write('Duración total: '), write(TiempoTotal), write(' dias'),
            nl, write('Costo total: '), write(CostoTotal), nl, nl
    ;   write('No existen actividades asociadas al destino dado.'), nl, nl
    ).


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
