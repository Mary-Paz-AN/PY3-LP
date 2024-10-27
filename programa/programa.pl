% Cargar la base de conocimiento 
:- consult('BC.pl').


% Entradas: Nombre, Descripcion
% Salida: Ninguna
% Restricciones: Ningunas
% Objetivo: Escribir el hecho de destino al archivo y a la base de conocimiento actual
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

% Entradas: Stream, Lista (La lista de categorias)
% Salida: Ninguna
% Restricciones: Ningunas
% Objetivo: Escribir en el archivo el las categorias en la lista
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

% Entradas: Nombre, Costo, Duracion, Descripcion, Tipos
% Salida: Ninguna
% Restricciones: Ningunas
% Objetivo: Guarda los hechos de actividad al archivo y a la base de conocimiento
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

% Entradas: Destino, Actividad
% Salida: Ninguna
% Restricciones: Ningunas
% Objetivo: Guarda los hechos de asociar_actividad al archivo y a la base de conocimiento
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


% Entradas: Destino
% Salidas: true o false
% Restricciones: Ninguna
% Objetivo: Verifica que el destino exista en la base de conocimiento
verificarDestino(Destino) :-
    destino(Destino, _).

% Entradas: Actividad
% Salidas: true o false
% Restricciones: Ninguna
% Verifica que la actividad exista en la base de conocimiento
verificarActividad(Actividad) :-
    actividad(Actividad, _, _, _, _).

% Entrada: Niniguna
% Salida: Ninguna
% Restricciones: Que el destino no exista. 
% Objetivo: Pregunta por la información del destino 
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

% Entrada: Niniguna
% Salida: Costo
% Restricciones: Que el costo dado sea entero. 
% Objetivo: Pregunta por el costo y verifica si es un entero y mayor a 0
preguntarXCostoActividad(Costo) :-
    write('Ingrese el costo de la actividad: '),
    read(CostoInput), nl,
    
    % Verificar si es un entero y mayor que 0
    (   integer(CostoInput),
        CostoInput > 0 ->
            Costo is CostoInput
    ;   (   integer(CostoInput) ->
                write('El costo debe ser mayor a 0.'), nl
        ;   write('El costo debe ser un número entero.'), nl
        ),
        preguntarXCostoActividad(Costo)
    ).

% Entrada: Niniguna
% Salida: Duracion
% Restricciones: Que la duracion sea entera 
% Pregunta por la verificar y verifica si es un entero y mayor a 0
preguntarXDuracionActividad(Duracion) :-
    write('Ingrese la duración de la actividad (días): '),
    read(DuracionInput), nl,
    
    % Verificar si es un entero y mayor que 0
    (   integer(DuracionInput),
        DuracionInput > 0 ->
            Duracion is DuracionInput
    ;   (   integer(DuracionInput) ->
                write('La duración debe ser mayor a 0.'), nl
        ;   write('La duración debe ser un número entero.'), nl
        ),
        preguntarXDuracionActividad(Duracion)
    ).

% Entrada: Elemento (a agregar), Lista (anterior)
% Salida: Lista (resultante)
% Restricciones: Ninguna
% Objetivo: Agrega una categoria a la lista
agregarCategoria(Elemento, Lista, [Elemento | Lista]).

% Entrada: ListaTipo (lista inicial)
% Salida: ListaTipoFinal (lista final)
% Restricciones: Que la categoria este entre las opciones
% Objetivo: Pregunta por la categoria que quiere agregar
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

% Entrada: Ninguna
% Salida: Ninguna
% Restricciones: Que la actividad no exista.
% Objetvo: Pregunta por la informacion de actividad 
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

% Entrada: Ninguna
% Salida: Actividad
% Restricciones: Que la actividad exista.
% Objetivo: Pregunta por la actividad y verificar que exista
preguntarActividadAA(Actividad) :-
    write('Ingrese el nombre de la actividad: '),
    read(ActividadInput), nl,

    (   verificarActividad(ActividadInput) ->
            Actividad = ActividadInput
    ;   write('La actividad no existe. Porfavor ingrese una existente.'), nl,
            preguntarActividadAA(Actividad)
    ).

% Entrada: Ninguna
% Salida: Ninguna
% Restricciones: Que el destino y la actividad existan.
% Objetivo: Pregunta por la información para asociar la actividad 
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

% Entrada: Ninguna
% Salida: Ninguna
% Restricciones: Que se elija una opcion valida.
% Objetivo: Menú para agregar hechos 
menuAgregarHechos :- 
    write('a. Destino'), nl,
    write('b. Actividad'), nl,
    write('c. Asociar Actividad'), nl,
    write('d. Volver'), nl,
    write('Ingrese la opcion que desea: '),
    read(Opcion), nl, nl,
    manejarMenuHechos(Opcion).

% Entrada: La opcion valida
% Salida: Ninguna
% Restricciones: Si no es valida dar un mensaje de error y volver al menu
% Objetivo: Maneja las opciones de agregar hechos 
manejarMenuHechos(a) :- agregarDestino, menuAgregarHechos.
manejarMenuHechos(b) :- agregarActividad, menuAgregarHechos.
manejarMenuHechos(c) :- agregarAsociarActividad, menuAgregarHechos.
manejarMenuHechos(d) :- !, true.
manejarMenuHechos(_) :- write('Opción inválida, vuelva a intentarlo.'), nl, nl, menuAgregarHechos.


% Entrada: Lista (Lista de los tipos)
% Salida: Ninguna
% Restricciones: Si ya va por el ultimo elmento no imprimir la coma
% Objetivo: Imprime las categorías de una actividad
mostrarTipos([Ultimo]) :-
    write(Ultimo).

mostrarTipos([Cabeza | Resto]) :-
    write(Cabeza),
    write(', '),
    mostrarTipos(Resto).

% Entradas: Lista (Lista de las actividades), TiempoAcumulado, CostoAcumulado
% Salidas: TiempoTotal, CostoTotal
% Restricciones: Si ya termino de recorrer la lista dar el costo y duracion total
% Objetivo: Imprime en pantalla las actividades dadas y acumula el costo y la duración
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


% Entrada: Ninguna
% Salida: Ninguna
% Restricciones: Si no hay actividades asociadas dar mensaje para informar al usuario
% Objetivo: Consultar destino
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

% Entrada: Ninguna
% Salida: Ningna
% Restricciones: Ninguna
% Objetivo: Preguntar por el tipo de actividad para ver actividades relacionadas al tipo
actividadXtipo :-
    write("Ingrese el tipo de actividad: "),
    read(Tipo),
    actividades_por_tipo(Tipo, Resultados),
    mostrar_resultados(Resultados).

% Entrada: Lista de las actividades por el tipo
% Salida: Ninguna
% Restricciones: Ninguna
% Objetivo: Mostrar los resultados de las actividades por el tipo dado
mostrar_resultados([]) :-
    write("No se encontraron actividades para este tipo."), nl.
mostrar_resultados([(Destino, Nombre, Costo, Duracion, Descripcion)|T]) :-
    write("Destino: "), write(Destino), nl,
    write("Actividad: "), write(Nombre), nl,
    write("Costo: "), write(Costo), nl,
    write("Duraci�n: "), write(Duracion), nl,
    write("Descripci�n: "), write(Descripcion), nl, nl,
    mostrar_resultados(T).


% Entrada: 
% Salida: 
% Restricciones: 
% Objetivo: Genenarar itinerario por monto 
itinerarioXmonto :- write("Genenarar itinerario por monto"), nl.


% Entrada: Ninguna
% Salida: Ninguna
% Restricciones: Que la opción sea valida
% Objetivo: Consultar actividades por precio. Pueden ser baratas o caras.
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

% Entrada: Monto
% Salida: Resultados (lista de las actividades)
% Restricciones: Ninguno
% Objetivo: Predicado para encontrar actividades más baratas
actividades_mas_baratas(Monto, Resultados) :-
    findall((Destino, Nombre, Costo, Duracion, Descripcion),
            (actividad(Nombre, Costo, Duracion, Descripcion, _),
             Costo =< Monto,
             asociar_actividad(Destino, Nombre)),
            Resultados).

% Entrada: Monto
% Salida: Resultados (lista de las actividades)
% Restricciones: Ninguno
% Objetivo: Predicado para encontrar actividades más caras
actividades_mas_caras(Monto, Resultados) :-
    findall((Destino, Nombre, Costo, Duracion, Descripcion),
            (actividad(Nombre, Costo, Duracion, Descripcion, _),
             Costo > Monto,
             asociar_actividad(Destino, Nombre)),
            Resultados).


% Entrada: 
% Salida: 
% Restricciones: 
% Objetivo: Genarar itinerario por dias 
itinerarioXdias :- write("Genarar itinerario por dias"), nl.


% Entrada: 
% Salida: 
% Restricciones: 
% Objetivo: Recomendar por frase 
recomendarXfrase :- write("Recomendar por frase"), nl.

% Entrada: Ninguna
% Salida: Actividades
% Restricciones: Ninguna
% Objetivo: Crea una lista de solo los nombres de las actividades
crearListaActividades(Actividades) :-
    findall(
        Actividad,
        actividad(Actividad, _, _, _, _),
        Actividades).

% Entrada: Actividad, Lista (anterior)
% Salida: NuevaLisa (resultante)
% Restricciones: Si la actividad no existe dentro de la lista la agrega
% Objetivo: Verificar si la actividad ya esta dentro de la lista
agregar_si_no_existe(Actividad, Lista, NuevaLista) :-
    (   member(Actividad, Lista) ->
        NuevaLista = Lista  
    ;   NuevaLista = [Actividad | Lista] 
    ).

% Entrada: Lista de actividades, Actividad (acumulada), CostoMax (acumulado)
% Salida: Ninguna
% Restricciones: Si hay actividades con un mismo costo maximo entonces se mostraran tambien
% Objetivo: Calcula la actividad más cara comprando costos
encontrarMasCara([], ActividadesMasCaras, _) :-
    % Imprime todas las actividades que comparten el costo máximo
    forall(member(Actividad, ActividadesMasCaras),
           (actividad(Actividad, Costo, Duracion, Descripcion, Tipo),
            write('Nombre: '), write(Actividad),
            nl, write('Costo: '), write(Costo),
            nl, write('Duración: '), write(Duracion), write(' días'),
            nl, write('Descripción: '), write(Descripcion),
            nl, write('Categorias: '), mostrarTipos(Tipo), nl, nl)).

encontrarMasCara([Cabeza | Resto], ActividadesMasCaras, CostoMax) :-
    actividad(Cabeza, Costo, _, _, _),
    (   Costo > CostoMax ->
        % Actualiza la lista solo con esta actividad si es más cara
        encontrarMasCara(Resto, [Cabeza], Costo)
    ;   Costo =:= CostoMax ->
        % Agrega la actividad a la lista si el costo es igual al máximo actual y no está repetida
        agregar_si_no_existe(Cabeza, ActividadesMasCaras, NuevaActividadesMasCaras),
        encontrarMasCara(Resto, NuevaActividadesMasCaras, CostoMax)
    ;   encontrarMasCara(Resto, ActividadesMasCaras, CostoMax)
    ).

% Entrada: Ninguna
% Salida: Niguna
% Restricciones: Ninguna
% Objetivo: Hace la consulta para buscar la actividad más cara
actividadMasCara :-
    crearListaActividades(Actividades),
    encontrarMasCara(Actividades, _, 0).

% Entrada: Lista de actividades, Actividad (acumulada), DuracionMin (acumulado)
% Salida: Ninguna
% Restricciones: Si hay actividades con una misma duracion minima entonces se mostraran tambien
% Objetivo: Calcula la actividad con menos duracion comparando cantidad de dias
encontrarMenorDuracion([], ActividadesMenorDuracion, _) :-
    % Imprime todas las actividades que comparten la duración mínima
    forall(member(Actividad, ActividadesMenorDuracion),
           (actividad(Actividad, Costo, Duracion, Descripcion, Tipo),
            write('Nombre: '), write(Actividad),
            nl, write('Costo: '), write(Costo),
            nl, write('Duración: '), write(Duracion), write(' días'),
            nl, write('Descripción: '), write(Descripcion),
            nl, write('Categorias: '), mostrarTipos(Tipo), nl, nl)).

encontrarMenorDuracion([Cabeza | Resto], ActividadesMenorDuracion, DuracionMin) :-
    actividad(Cabeza, _, Duracion, _, _),
    (   Duracion < DuracionMin ->
        % Actualiza la lista solo con esta actividad si es de menor duración
        encontrarMenorDuracion(Resto, [Cabeza], Duracion)
    ;   Duracion =:= DuracionMin ->
        % Agrega la actividad a la lista si la duración es igual a la mínima actual y no está repetida
        agregar_si_no_existe(Cabeza, ActividadesMenorDuracion, NuevaActividadesMenorDuracion),
        encontrarMenorDuracion(Resto, NuevaActividadesMenorDuracion, DuracionMin)
    ;   encontrarMenorDuracion(Resto, ActividadesMenorDuracion, DuracionMin)
    ).

% Entrada: Ninguna
% Salida: Niguna
% Restricciones: Ninguna
% Objetivo: Hace la consulta para buscar la actividad con menos duración
actividadMenorDuracaion :-
    crearListaActividades(Actividades),
    encontrarMenorDuracion(Actividades, _, 999).

% Entrada: Ninguna
% Salida: Niguna
% Restricciones: Ninguna
% Objetivo: Presentar el menu de estadisticas
menuEstadisticas :- 
    write('a. Top 3 ciudades con más actividades'), nl,
    write('b. Actividad más cara.'), nl,
    write('c. Actividad de menor duración'), nl,
    write('d. Categoría con más actividades'), nl,
    write('e. Volver'), nl,
    write('Ingrese la opcion que desea: '),
    read(Opcion), nl, nl,
    manejarMenuEstadisticas(Opcion).

% Entrada: Opciones validas
% Salida: Niguna
% Restricciones: Que la opcion sea valida, si no dar mensaje de error.
% Objetivo: Manejar las opciones del menú de estadisticas
manejarMenuEstadisticas(a) :- write('Top 3'), nl, menuEstadisticas.
manejarMenuEstadisticas(b) :- actividadMasCara, nl, menuEstadisticas.
manejarMenuEstadisticas(c) :- actividadMenorDuracaion, nl, menuEstadisticas.
manejarMenuEstadisticas(d) :- write('Cat mas actividades'), nl, menuEstadisticas.
manejarMenuEstadisticas(e) :- !, true.
manejarMenuEstadisticas(_) :- write('Opción inválida, vuelva a intentarlo.'), nl,nl, menuEstadisticas.


% Entrada: 
% Salida: 
% Restricciones: 
% Objetivo: 
actividades_por_tipo(Tipo, Resultados) :-
    findall((Destino, Nombre, Costo, Duracion, Descripcion),
            (actividad(Nombre, Costo, Duracion, Descripcion, Tipos),
             member(Tipo, Tipos),
             asociar_actividad(Destino, Nombre)),
            Resultados).


% Entrada: Ninguna
% Salida: Ninguna
% Restricciones: Ninguna
% Objetivo: Enseñar las opciones del menú
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
    manejarMenu(Opcion).

% Entrada: Opciones validas
% Salida: Ninguna
% Restricciones: Que la opcion dada sea valida, si no dar mensaje de error
% Objetivo: Manejar las opciones del menú
manejarMenu(a) :- menuAgregarHechos, menu.
manejarMenu(b) :- consultarDestino, menu.
manejarMenu(c) :- actividadXtipo, menu.
manejarMenu(d) :- consultaXprecio, menu.
manejarMenu(e) :- itinerarioXmonto, menu.
manejarMenu(f) :- itinerarioXdias, menu.
manejarMenu(g) :- recomendarXfrase, menu.
manejarMenu(h) :- menuEstadisticas, menu.
manejarMenu(j) :- !, true.
manejarMenu(_) :- write('Opción inválida, vuelva a intentarlo.'), nl,nl, menu.


% Entrada: Ninguna
% Salida: Ninguna
% Restricciones: Ninguna
% Objetivo: Enseña las opciones del menú principal 
menuPrincipal :-
    write('m. Menú'), nl,
    write('s. Salir'), nl,
    write('Ingrese la opcion que desea: '),
    read(Opcion), nl, nl,
    manejarMenuPrincipal(Opcion).

% Entrada: Opciones validas
% Salida: Ninguna
% Restricciones: Si la opción no es valida dar un mensaje de error
% Objetivo: Manejar las opciones del menú principal
manejarMenuPrincipal(m) :- menu, menuPrincipal.
manejarMenuPrincipal(s) :- write('Saliendo del programa...'), nl, true.
manejarMenuPrincipal(_) :- write('Opción inválida, vuelva a intentarlo.'), nl, nl, menuPrincipal.


% Entrada: Ninguna
% Salida: Ninguna
% Restricciones: Ninguna
% Objetivo: iniciar el programa 
iniciar :- menuPrincipal.
