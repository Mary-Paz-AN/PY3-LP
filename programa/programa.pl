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
% Objetivo: Pregunta por la informacion del destino 
agregarDestino :- 
    write('Ingrese el nombre del destino: '),
    read(Nombre), nl,
    (   verificarDestino(Nombre) -> 
            write('El destino ya existe. Porfavor agregue otro.'), nl,
            agregarDestino
    ;   write('Escriba una breve descripcion del destino: '), 
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
        ;   write('El costo debe ser un numero entero.'), nl
        ),
        preguntarXCostoActividad(Costo)
    ).

% Entrada: Niniguna
% Salida: Duracion
% Restricciones: Que la duracion sea entera 
% Pregunta por la verificar y verifica si es un entero y mayor a 0
preguntarXDuracionActividad(Duracion) :-
    write('Ingrese la duracion de la actividad (dias): '),
    read(DuracionInput), nl,
    
    % Verificar si es un entero y mayor que 0
    (   integer(DuracionInput),
        DuracionInput > 0 ->
            Duracion is DuracionInput
    ;   (   integer(DuracionInput) ->
                write('La duracion debe ser mayor a 0.'), nl
        ;   write('La duracion debe ser un numero entero.'), nl
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
    write('6. Diversion'), nl,
    write('7. Romantico'), nl,
    write('8. Gastronomia'), nl,
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
                write('Categorias añadidas exitosamente.'), nl
            ;   write('Agregue al menos una categoria antes de salir.'), nl,
                preguntarXTipoActividad(ListaTipo, ListaTipoFinal)
            )
    ;   write('Opcion invalida. Vuelva a intentarlo.'), nl,
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
% Objetivo: Pregunta por la informacion para asociar la actividad 
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
% Objetivo: Menu para agregar hechos 
menuAgregarHechos :- 
    write('----- Agregar Hechos -----'), nl,
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
manejarMenuHechos(_) :- write('Opcion invalida, vuelva a intentarlo.'), nl, nl, menuAgregarHechos.


% Entrada: Lista (Lista de los tipos)
% Salida: Ninguna
% Restricciones: Si ya va por el ultimo elmento no imprimir la coma
% Objetivo: Imprime las categorias de una actividad
mostrarTipos([Ultimo]) :-
    write(Ultimo).

mostrarTipos([Cabeza | Resto]) :-
    write(Cabeza),
    write(', '),
    mostrarTipos(Resto).

% Entradas: Lista (Lista de las actividades), TiempoAcumulado, CostoAcumulado
% Salidas: TiempoTotal, CostoTotal
% Restricciones: Si ya termino de recorrer la lista dar el costo y duracion total
% Objetivo: Imprime en pantalla las actividades dadas y acumula el costo y la duracion
mostrarActividades([], TiempoTotal, CostoTotal, TiempoTotal, CostoTotal). 

mostrarActividades([Cabeza | Resto], TiempoAcum, CostoAcum, TiempoTotal, CostoTotal) :-
    actividad(Cabeza, Costo, Duracion, Descripcion, Tipos), 
    write('Nombre: '), write(Cabeza),
    nl, write('Costo: '), write(Costo),
    nl, write('Duracion: '), write(Duracion), write(' dias'),
    nl, write('Descripcion: '), write(Descripcion),
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

    % Verificar que el destino exista
    (   verificarDestino(Destino) ->
            % Encuentra todas las actividades asociadas al destino
            findall(
                Actividad,
                asociar_actividad(Destino, Actividad),
                Actividades),
            
            % Verificar que existan actividades asociadas
            (   Actividades \= [] ->
                    mostrarActividades(Actividades, 0, 0, TiempoTotal, CostoTotal),
                    nl, write('Duracion total: '), write(TiempoTotal), write(' dias'),
                    nl, write('Costo total: '), write(CostoTotal), nl, nl
            ;   write('No existen actividades asociadas al destino dado.'), nl, nl
            )
    ;   write('El destino ingresado no existe'), nl, nl
    ).

% Entrada: Ninguna
% Salida: Ninguna
% Restricciones: Ninguna
% Objetivo: Preguntar al usuario por un tipo de actividad y mostrar actividades relacionadas a ese tipo.
actividadXtipo :-
    write("Ingrese el tipo de actividad: "),
    read(Tipo),
    actividades_por_tipo(Tipo, Resultados),
    mostrar_resultados(Resultados).

% Entrada: Tipo (cadena)
% Salida: Resultados (lista de actividades relacionadas al tipo)
% Restricciones: El tipo debe coincidir con alguno existente en las actividades o en su afinidad.
% Objetivo: Buscar actividades que coincidan con el tipo ingresado o que tengan afinidad con ese tipo.
actividades_por_tipo(Tipo, Resultados) :-
    findall((Destino, Nombre, Costo, Duracion, Descripcion),
            (actividad(Nombre, Costo, Duracion, Descripcion, Tipos),
             (member(Tipo, Tipos) ; (afinidad(Tipo, TipoAfin), member(TipoAfin, Tipos))),
             asociar_actividad(Destino, Nombre)),
            Resultados).

% Entrada: Resultados (lista de actividades por tipo)
% Salida: Ninguna
% Restricciones: Ninguna
% Objetivo: Mostrar la lista de actividades relacionadas al tipo de actividad ingresado.
mostrar_resultados([]) :-
    write("Lista de actividades generada."), nl,
    write("Si esta vacio, no se encontraron actividades de esta categoria o afines."), nl.
mostrar_resultados([(Destino, Nombre, Costo, Duracion, Descripcion)|T]) :-
    write("Destino: "), write(Destino), nl,
    write("Actividad: "), write(Nombre), nl,
    write("Costo: "), write(Costo), nl,
    write("Duración: "), write(Duracion), nl,
    write("Descripción: "), write(Descripcion), nl, nl,
    mostrar_resultados(T).

% Entrada: Ninguna
% Salida: Genera un itinerario de actividades basado en el monto máximo, categoría de preferencia, cantidad de personas y preferencia de estancia.
% Restricciones: El monto debe ser un número positivo; la categoría debe existir en la base de conocimiento.
% Objetivo: Crear un itinerario de actividades según el presupuesto especificado.
itinerarioXmonto :-
    write("Ingrese el monto maximo: "),
    read(MontoMax),
	(integer(MontoMax) -> true ; write("Error: El monto maximo debe ser un numero entero."), nl, fail),
	
    write("Ingrese la categoria de preferencia: "),
    read(Categoria),
	
    write("Ingrese la cantidad de personas: "),
    read(Personas),
	(integer(Personas) -> true ; write("Error: La cantidad de personas debe ser un numero entero."), nl, fail),
	
    write("Prefiere estancias (l)argas o (c)ortas? "),
    read(PreferenciaEstancia),
	(atom(PreferenciaEstancia) -> true ; write("Error: La preferencia de estancia debe ser un valor tipo atomo (l o c)."), nl, fail),
	
    generar_itinerario(MontoMax, Categoria, Personas, PreferenciaEstancia, Itinerario),
    mostrar_itinerario(Itinerario).

% Entrada: MontoMax (número), Categoria (cadena), Personas (número), PreferenciaEstancia ('l' o 'c')
% Salida: Itinerario generado con actividades filtradas y ordenadas según duración y presupuesto.
% Restricciones: El monto máximo y el número de personas deben ser mayores a cero; la categoría debe existir en las actividades.
% Objetivo: Generar un itinerario que se ajuste a los criterios de presupuesto y preferencia de estancia.
generar_itinerario(MontoMax, Categoria, Personas, PreferenciaEstancia, Itinerario) :-
    findall((Destino, Nombre, Costo, Duracion, Descripcion, Tipos),
            (actividad(Nombre, Costo, Duracion, Descripcion, Tipos),
             asociar_actividad(Destino, Nombre)),
            Actividades),
    filtrar_por_categoria(Actividades, Categoria, ActividadesFiltradas),
    ordenar_por_duracion(ActividadesFiltradas, PreferenciaEstancia, ActividadesOrdenadas),
    seleccionar_actividades(ActividadesOrdenadas, MontoMax, Personas, Itinerario).

% Entrada: Actividades (lista), Categoria (cadena)
% Salida: Lista de actividades que coinciden con la categoría indicada.
% Restricciones: La categoría debe ser válida y estar presente en las actividades.
% Objetivo: Filtrar actividades según coincidencia con la categoría indicada.
filtrar_por_categoria(Actividades, Categoria, ActividadesFiltradas) :-
    include(tiene_categoria(Categoria), Actividades, ActividadesFiltradas).

% Entrada: Categoria (cadena), Actividad (tupla con tipos de actividad)
% Salida: Verdadero si la actividad contiene la categoría especificada.
% Restricciones: Ninguna
% Objetivo: Verificar si una actividad pertenece a la categoría especificada.
tiene_categoria(Categoria, (_, _, _, _, _, Tipos)) :-
    member(Categoria, Tipos).

% Entrada: Actividad (tupla)
% Salida: Pares de duración y actividad
% Restricciones: Ninguna
% Objetivo: Extraer la duración y combinarla con la actividad para ordenarlas.
extraer_duracion((Ciudad, Actividad, Precio, Duracion, Descripcion, Categorias), Duracion-(Ciudad, Actividad, Precio, Duracion, Descripcion, Categorias)).

% Entrada: Actividades (lista), PreferenciaEstancia ('l' o 'c')
% Salida: Lista de actividades ordenadas por duración de mayor a menor o de menor a mayor.
% Restricciones: Ninguna
% Objetivo: Ordenar actividades según la duración en función de la preferencia de estancia.
ordenar_por_duracion(Actividades, l, ActividadesOrdenadas) :-
    maplist(extraer_duracion, Actividades, ActividadesConDuracion),
    sort(1, @>=, ActividadesConDuracion, ActividadesConDuracionOrdenada),
    pairs_values(ActividadesConDuracionOrdenada, ActividadesOrdenadas).

ordenar_por_duracion(Actividades, c, ActividadesOrdenadas) :-
    maplist(extraer_duracion, Actividades, ActividadesConDuracion),
    sort(1, @=<, ActividadesConDuracion, ActividadesConDuracionOrdenada),
    pairs_values(ActividadesConDuracionOrdenada, ActividadesOrdenadas).

% Entrada: Lista de actividades, Monto máximo, Cantidad de personas
% Salida: Subconjunto de actividades que no superen el monto máximo permitido.
% Restricciones: El costo total no debe superar el monto máximo.
% Objetivo: Seleccionar actividades para el itinerario dentro del presupuesto.
seleccionar_actividades([], _, _, []).
seleccionar_actividades([(Destino, Nombre, Costo, Duracion, Descripcion, _)|T], MontoMax, Personas, [(Destino, Nombre, Costo, Duracion, Descripcion)|Itinerario]) :-
    CostoTotal is Costo * Personas,
    CostoTotal =< MontoMax,
    NuevoMontoMax is MontoMax - CostoTotal,
    seleccionar_actividades(T, NuevoMontoMax, Personas, Itinerario).
seleccionar_actividades([_|T], MontoMax, Personas, Itinerario) :-
    seleccionar_actividades(T, MontoMax, Personas, Itinerario).

% Entrada: Itinerario (lista)
% Salida: Muestra el itinerario completo o indica si no hay actividades que coincidan con el filtro.
% Restricciones: Ninguna
% Objetivo: Mostrar el itinerario generado con las actividades seleccionadas.
mostrar_itinerario([]) :-
    write("Itinerario hecho."), nl,
    write("En caso de vacio (No se encontraron actividades que coincidan con el filtro)."), nl.
mostrar_itinerario([(Destino, Nombre, Costo, Duracion, Descripcion)|T]) :-
    write("Destino: "), write(Destino), nl,
    write("Actividad: "), write(Nombre), nl,
    write("Costo: "), write(Costo), nl,
    write("Duracion: "), write(Duracion), nl,
    write("Descripcion: "), write(Descripcion), nl, nl,
    mostrar_itinerario(T).


% Entrada: Ninguna
% Salida: Ninguna
% Restricciones: Que la opcion sea valida
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
% Objetivo: Predicado para encontrar actividades mas baratas
actividades_mas_baratas(Monto, Resultados) :-
    findall((Destino, Nombre, Costo, Duracion, Descripcion),
            (actividad(Nombre, Costo, Duracion, Descripcion, _),
             Costo =< Monto,
             asociar_actividad(Destino, Nombre)),
            Resultados).

% Entrada: Monto
% Salida: Resultados (lista de las actividades)
% Restricciones: Ninguno
% Objetivo: Predicado para encontrar actividades mas caras
actividades_mas_caras(Monto, Resultados) :-
    findall((Destino, Nombre, Costo, Duracion, Descripcion),
            (actividad(Nombre, Costo, Duracion, Descripcion, _),
             Costo > Monto,
             asociar_actividad(Destino, Nombre)),
            Resultados).


% Entrada: Ninguna
% Salida: Muestra un itinerario de actividades basado en la cantidad de días, categoría de preferencia y preferencia de estancia.
% Restricciones: La cantidad de días debe ser un número entero; la categoría debe existir en la base de conocimiento.
% Objetivo: Generar un itinerario de actividades basado en los parámetros del usuario con opción de regeneración.
itinerarioXdias :-
    write("Ingrese la cantidad maxima de dias: "),
    read(MaxDias),
    (integer(MaxDias) -> true ; write("Error: La cantidad de dias debe ser un numero entero."), nl, fail),
    
    write("Ingrese la categoria de preferencia: "),
    read(Categoria),
    
    write("Prefiere estancias (l)argas o (c)ortas? "),
    read(PreferenciaEstancia),
    (atom(PreferenciaEstancia) -> true ; write("Error: La preferencia de estancia debe ser un valor tipo atomo (l o c)."), nl, fail),
    
    generar_itinerario_por_dias(MaxDias, Categoria, PreferenciaEstancia),
    preguntar_regenerar.

% Entrada: MaxDias (entero), Categoria (cadena), PreferenciaEstancia ('l' o 'c')
% Salida: Genera y muestra un itinerario de actividades basado en los parámetros dados.
% Restricciones: MaxDias debe ser mayor a cero; Categoria debe existir en las actividades.
% Objetivo: Crear o regenerar el itinerario con las actividades seleccionadas según los datos del usuario.
generar_itinerario_por_dias(MaxDias, Categoria, PreferenciaEstancia) :-
    obtener_actividades(Categoria, PreferenciaEstancia, ActividadesOrdenadas),
    seleccionar_actividades_por_dias(ActividadesOrdenadas, MaxDias, Itinerario),
    mostrar_itinerario_dias(Itinerario),
    % Guardamos los datos para regenerar después
    retractall(datos_itinerario(_, _, _)),  % Limpiamos cualquier dato previo
    assertz(datos_itinerario(MaxDias, Categoria, PreferenciaEstancia)).

% Entrada: Categoria (cadena), PreferenciaEstancia ('l' o 'c')
% Salida: Lista de actividades ordenadas por duración o aleatoriedad.
% Restricciones: Categoria debe existir en las actividades.
% Objetivo: Obtener actividades filtradas por categoría o afinidad, y ordenarlas según la preferencia de estancia.
obtener_actividades(Categoria, PreferenciaEstancia, ActividadesOrdenadas) :-
    findall((Destino, Nombre, Costo, Duracion, Descripcion, Tipos),
            (actividad(Nombre, Costo, Duracion, Descripcion, Tipos),
             asociar_actividad(Destino, Nombre)),
            Actividades),
    filtrar_por_categoria_afin(Actividades, Categoria, ActividadesFiltradas),
    ordenar_por_duracion_dias(ActividadesFiltradas, PreferenciaEstancia, ActividadesOrdenadas).

% Entrada: Actividades (lista), Categoria (cadena)
% Salida: Lista de actividades filtradas que incluyen la categoría o afinidad especificada.
% Restricciones: Categoria debe ser válida en afinidades o en la lista de actividades.
% Objetivo: Filtrar actividades según coincidencia directa con la categoría o con afinidades definidas.
filtrar_por_categoria_afin(Actividades, Categoria, ActividadesFiltradas) :-
    include(tiene_categoria_o_afin(Categoria), Actividades, ActividadesFiltradas).

% Entrada: Categoria (cadena), Datos de actividad
% Salida: Verdadero si la actividad incluye la categoría o una afinidad.
% Restricciones: Ninguna
% Objetivo: Determinar si una actividad coincide con la categoría indicada o su afinidad.
tiene_categoria_o_afin(Categoria, (_, _, _, _, _, Tipos)) :-
    (member(Categoria, Tipos) ; (member(CategoriaAf, Tipos), afinidad(Categoria, CategoriaAf))).

% Entrada: Actividades (lista), PreferenciaEstancia ('l' o 'c')
% Salida: Actividades ordenadas por duración en orden ascendente o descendente.
% Restricciones: Ninguna
% Objetivo: Ordenar las actividades por duración en base a la preferencia de estancia.
ordenar_por_duracion_dias(Actividades, l, ActividadesOrdenadas) :-
    maplist(extraer_duracion, Actividades, ActividadesConDuracion),
    random_permutation(ActividadesConDuracion, ActividadesRandomizadas), % Variacion para regenerar
    sort(1, @>=, ActividadesRandomizadas, ActividadesConDuracionOrdenada),
    pairs_values(ActividadesConDuracionOrdenada, ActividadesOrdenadas).

ordenar_por_duracion_dias(Actividades, c, ActividadesOrdenadas) :-
    maplist(extraer_duracion, Actividades, ActividadesConDuracion),
    random_permutation(ActividadesConDuracion, ActividadesRandomizadas), % Variacion para regenerar
    sort(1, @=<, ActividadesRandomizadas, ActividadesConDuracionOrdenada),
    pairs_values(ActividadesConDuracionOrdenada, ActividadesOrdenadas).

% Entrada: Lista de actividades, número máximo de días
% Salida: Subconjunto de actividades que no superan el número máximo de días
% Restricciones: La duración total no debe exceder el número máximo de días.
% Objetivo: Seleccionar actividades para un itinerario que se ajuste al límite de días.
seleccionar_actividades_por_dias([], _, []).
seleccionar_actividades_por_dias([(Destino, Nombre, Costo, Duracion, Descripcion, _)|T], DiasMax, [(Destino, Nombre, Costo, Duracion, Descripcion)|Itinerario]) :-
    Duracion =< DiasMax,
    NuevoDiasMax is DiasMax - Duracion,
    seleccionar_actividades_por_dias(T, NuevoDiasMax, Itinerario).
seleccionar_actividades_por_dias([_|T], DiasMax, Itinerario) :-
    seleccionar_actividades_por_dias(T, DiasMax, Itinerario).

% Entrada: Itinerario (lista)
% Salida: Muestra el itinerario completo o indica si no hay actividades que coincidan con el filtro.
% Restricciones: Ninguna
% Objetivo: Mostrar las actividades seleccionadas en el itinerario.
mostrar_itinerario_dias([]) :-
    write("Itinerario completo."), nl,
    write("Si esta vacio, no se encontraron actividades que coincidan con el filtro."), nl.
mostrar_itinerario_dias(Itinerario) :-
    mostrar_actividades(Itinerario).

% Entrada: Lista de actividades
% Salida: Muestra los detalles de cada actividad en la lista.
% Restricciones: Ninguna
% Objetivo: Desplegar la información de cada actividad en el itinerario.
mostrar_actividades([]).
mostrar_actividades([(Destino, Nombre, Costo, Duracion, Descripcion)|T]) :-
    write("Destino: "), write(Destino), nl,
    write("Actividad: "), write(Nombre), nl,
    write("Costo: "), write(Costo), nl,
    write("Duracion: "), write(Duracion), nl,
    write("Descripcion: "), write(Descripcion), nl, nl,
    mostrar_actividades(T).

% Entrada: Ninguna
% Salida: Llama al predicado regenerar si el usuario desea un nuevo itinerario.
% Restricciones: Ninguna
% Objetivo: Preguntar al usuario si desea regenerar el itinerario.
preguntar_regenerar :-
    write("¿Desea regenerar el itinerario? (y/n): "),
    read(Opcion),
    (Opcion == y -> regenerar ; true).

% Entrada: Ninguna
% Salida: Genera nuevamente el itinerario con los datos previos guardados.
% Restricciones: Ninguna
% Objetivo: Volver a generar el itinerario utilizando los mismos datos de entrada inicial.
regenerar :-
    datos_itinerario(MaxDias, Categoria, PreferenciaEstancia),
    generar_itinerario_por_dias(MaxDias, Categoria, PreferenciaEstancia).


% Entrada: Ninguna
% Salida: Actividades
% Restricciones: Ninguna
% Objetivo: Crea una lista de solo los nombres de las actividades
crearListaActividades(Actividades) :-
    findall(
        Actividad,
        actividad(Actividad, _, _, _, _),
        Actividades).

% Entrada: Ninguna
% Salida: Destinos
% Restricciones: Ninguna
% Objetivo: rear una lista de todos los destinos en la BC
crearListaDestinos(Destinos) :-
    findall(
        Destino,
        destino(Destino, _),
        Destinos).

% Entrada: Lista (lista de palabras), ListaAcum (lista que se acumula)
% Salida: ListaResultante (Lista con los datos objetivos)
% Restricciones: Si la palabra tiene menos de 4 letras no se incluye e la lista
% Objetivo: Eliminar palabras innecesarias en la lista de palabras
eliminarArticulos([], ListaAcum, ListaResultante) :-
    % Se invierte para respetar el orden original de la frase
    reverse(ListaAcum, ListaResultante).

eliminarArticulos([Cabeza | Resto], ListaAcum, ListaResultante) :-
    string_length(Cabeza, Tamano),
    % Si el largo es menor a 4 entonces o se agrega a la lista
    (   Tamano < 4 ->  
    		eliminarArticulos(Resto, ListaAcum, ListaResultante)
    ;   % Si es mayor o igual a 4 se agrega a la lista resultante
    	eliminarArticulos(Resto, [Cabeza | ListaAcum], ListaResultante)
    ).

% Entrada: ListaAtomos (lista de atomos de los hechos)
% Salida: ListaPalabras (lista del atomo convertido en string)
% Restricciones: Ninguna
% Objetivo: Crear una lista de los atomos como string
separarAtomoLista(ListaAtomos, ListaPalabras) :-
    % Usar maplist para aplicar el predicado de separación a cada átomo de la lista
    maplist(separarAtomo, ListaAtomos, ListasPalabrasTemporales),
    % Convertir 
    flatten(ListasPalabrasTemporales, ListaPalabras).

% Entrada: Atom
% Salida: ListaPalabras (lista del atomo convertido en string)
% Restricciones: Ninguna
% Objetivo: Separar aquellos atomos con la barra inferior.
separarAtomo(Atom, ListaPalabras) :-
    % Convertir el átomo a string
    atom_string(Atom, String),
    
    % Separar el por _
    split_string(String, "_", " ", ListaStrings),
    ListaPalabras = ListaStrings.


% Entrada: Palabra
% Salida: Actividades (Lista de los nombres de las actividades)
% Restricciones: Ninguna
% Objetivo: Buscar las actividades con la categoria especificada.
esCategoria(Palabra, Actividades) :-
    atom_string(AtmPalabra, Palabra),
    findall(Nombre,
            (actividad(Nombre, _, _, _, Categorias),
             tiene_categoria_o_afin(AtmPalabra, (_, _, _, _, Categorias))),
            Actividades).

% Entrada: Lista (La lista de las palabras de la frase)
% Salida: Contador(Contador para las actividades encontradas)
% Restricciones: Ninguna
% Objetivo: Buscar las actividades con la categoria especificada.
buscarActividadTipos([], Contador) :- 
    (Contador = 0 -> false; true).

buscarActividadTipos([Cabeza | Cola], Contador) :-
    (   esCategoria(Cabeza, Actividades),
        Actividades \= [] ->  
        mostrarActividades(Actividades, 0, 0, _, _),
        Count is Contador + 1,
        buscarActividadTipos(Cola, Count)
    ;   % Si no coincide
        buscarActividadTipos(Cola, Contador)
    ).

% Entrada: Palabra (Palabra de la que se busca en la descripcion)
% Salida: Nombre, Descripcion (Informacion del destino)
% Restricciones:  Ninguna
% Objetivo: Verifica si la palbra es parte de la descripcion de algun destino
esDestDescp(Palabra, Nombre, Descripcion) :-
    destino(Nombre,Descripcion),
    sub_string(Descripcion, _, _, _, Palabra).


% Entrada: Lista de palabras a buscar
% Salida: Contador (Indica cuantas destinos se encontraron)
% Restricciones: Si el contador es cero devolver false. Si un destino no tiene actividad asociada indicar al usuario
% Objetivo: Buscar coicidencias de las palabras de la lista con la descripcion de los destinos
buscarDestinoDescripcion([], Contador) :- 
    (Contador = 0 -> false; true).

buscarDestinoDescripcion([Cabeza | Cola], Contador) :-
    (   esDestDescp(Cabeza, Destino, Descripcion) ->  
            write('Se encontro un destino con su descripcion'), nl,
            write('Nombre Destino: '), write(Destino), nl,
            write('Descripcion: '), write(Descripcion), nl,
            
            % Busca las actividades asociadas al destino
            findall(
                Actividad,
                asociar_actividad(Destino, Actividad),
                Actividades),
            
            % Verificar que existan actividades asociadas
            (   Actividades \= [] ->
                    write('Actividades asociadas:'), nl,
                    mostrarActividades(Actividades, 0, 0, _, _)
            ;   write('No existen actividades asociadas al destino.'), nl, nl
            ),
            Count is Contador + 1,
            buscarDestinoDescripcion(Cola, Count)
    ;   % Si no se encuentra concidencias
        buscarDestinoDescripcion(Cola, Contador)
    ).

% Entrada: Palabra (La palabra a buscar en la descripcion)
% Salida: Nombre, Monto, Duracion, Descripcion, Tipos (Info de la actividad)
% Restricciones: Ninguna
% Objetivo: Verifica si la palbra es parte de la descripcion de alguna actividad
esDesAct(Palabra, Nombre, Monto, Duracion, Descripcion, Tipos) :-
    actividad(Nombre, Monto, Duracion, Descripcion, Tipos),
    sub_string(Descripcion, _, _, _, Palabra).

% Entrada: Lista (Lista de las palabras de la frase)
% Salida: Contador (Indica cuantas actividades se encontraron)
% Restricciones: Si el contador es cero devolver false. 
% Objetivo: Buscar coicidencias de las palabras de la lista con la descripcion ed las actividades
buscarActividadDescripcion([], Contador) :- 
    (Contador = 0 -> false; true).

buscarActividadDescripcion([Cabeza | Cola], Contador) :-
    (   esDesAct(Cabeza, Nombre, Monto, Duracion, Descripcion, Tipos) ->  
            write('Nombre: '), write(Nombre), nl,
            write('Monto: '), write(Monto), nl,
            write('Duracion: '), write(Duracion), nl,
            write('Descripcion: '), write(Descripcion), nl,
            write('Categorias: '), mostrarTipos(Tipos), nl, nl,
            Count is Contador + 1,
            buscarActividadDescripcion(Cola, Count)
    ;   % Si no se encuentra concidencias
        buscarActividadDescripcion(Cola, Contador)
    ).

% Entrada: Lista de palabras a buscar, ListaAct (lista de destinos en la BC)
% Salida: Contador (Indica cuantas destinos se encontraron)
% Restricciones: Si el contador es cero devolver false. Si un destino no tiene actividad asociada indicar al usuario
% Objetivo: Buscar coicidencias de las palabras de la lista con el nombre de los destinos
compararDestinos([], _, Contador) :- 
    (Contador = 0 -> false; true).

compararDestinos([Cabeza | Resto], ListAct, Contador) :-
    % Verificar si hay una actividad que contiene la palabra
    (   member(Cabeza, ListAct) ->
            Count is Contador + 1,
            atom_string(AtomPalabra, Cabeza),
            destino(Destino, Descripcion),
            sub_atom(Destino, _, _, _, AtomPalabra),
            write('Se encontro un destino con su descripcion'), nl,
            write('Nombre Destino: '), write(Destino), nl,
            write('Descripcion: '), write(Descripcion), nl,
            
            % Busca las actividades asociadas al destino
            findall(
                Actividad,
                asociar_actividad(Destino, Actividad),
                Actividades),
            
            % Verificar que existan actividades asociadas
            (   Actividades \= [] ->
                    write('Actividades asociadas:'), nl,
                    mostrarActividades(Actividades, 0, 0, _, _)
            ;   write('No existen actividades asociadas al destino.'), nl, nl
            ),
            compararDestinos(Resto, ListAct, Count)
    ;   % Si no se encuentra
        compararDestinos(Resto, ListAct, Contador)
    ).

% Entrada: ListaFrase
% Salida: Ninguna
% Restricciones: Ninguna
% Objetivo: Buscar una actividad por medio del nombre de un destino
buscarDestinoNombre(ListaFrase) :-
    crearListaDestinos(Destinos),
    separarAtomoLista(Destinos, ListaDestinosStr),
    eliminarArticulos(ListaDestinosStr, [], ListDest),
    compararDestinos(ListaFrase, ListDest, 0). 

% Entrada: Lista de palabras a buscar, ListaAct (lista de actiidades enla BC)
% Salida: Contador (Indica cuantas actividades se encontraron)
% Restricciones: Si el contaor es cero deolver false
% Objetivo: Buscar coicidencias de las palabras de la lista con el nombre de las actividades
compararActividades([], _, Contador) :- 
    (Contador = 0 -> false; true).

compararActividades([Cabeza | Resto], ListAct, Contador) :-
    % Verificar si hay una actividad que contiene la palabra
    (   member(Cabeza, ListAct) ->
        	Count is Contador + 1,
        	atom_string(AtomPalabra, Cabeza),
        	actividad(Nombre, Monto, Duracion, Descripcion, Tipos),
        	sub_atom(Nombre, _, _, _, AtomPalabra),
        	write('Nombre: '), write(Nombre), nl,
        	write('Monto: '), write(Monto), nl,
            write('Duracion: '), write(Duracion), nl,
            write('Descripcion: '), write(Descripcion), nl,
            write('Categorias: '), mostrarTipos(Tipos), nl, nl,
        	compararActividades(Resto, ListAct, Count)
    ;   % Si no se encuentra
    	compararActividades(Resto, ListAct, Contador)
    ).

% Entrada: ListaFrase
% Salida: Ninguna
% Restricciones: Ninguna
% Objetivo: Buscar una actividad por medio del nombre de la actividad
buscarActividadNombre(ListaFrase) :-
    crearListaActividades(Actividades),
    separarAtomoLista(Actividades, ListaActividadesStr),
    eliminarArticulos(ListaActividadesStr, [], ListAct),
    compararActividades(ListaFrase, ListAct, 0). 

% Entrada: Ninguna
% Salida: Ninguna
% Restricciones: Si no se encuentran actividades informar al usuario
% Objetivo: Recomendar actividades por medio de frases del usuarios
recomendarXFrase :-
    write('Escriba una breve descripcion de lo que quisiera:'), nl,
    read(Frase), nl,
    atom_string(Frase, FraseStr),
    split_string(FraseStr, " ", ";.", ListaPalabras),
    eliminarArticulos(ListaPalabras, [], ListaFrase),
    (   buscarActividadNombre(ListaFrase)
    ;   buscarDestinoNombre(ListaFrase)
    ;   buscarActividadDescripcion(ListaFrase, 0)
    ;   buscarDestinoDescripcion(ListaFrase, 0)
    ;   buscarActividadTipos(ListaFrase, 0)
    ) -> true ; write('No se encontraron actividades.'), nl, nl.


% Entrada: Lista de destinos
% Salida: Ninguna
% Restricciones: Si hay el tamaño de la lista es menor a 3 solo se imprime los que hay
% Objetivo: Imprime en pantalla el top 3
getTopCuidades([], _, _) :- !.
getTopCuidades([[Destino, Cantidad] | Resto], Contador, Objetivo) :- 
    (   Contador < Objetivo ->
            % Imprime hasta 3 destinos
            Conta is Contador + 1,
            destino(Destino, Descripcion),
            write('Nombre: '), write(Destino), nl,
            write('Descripción: '), write(Descripcion), nl,
            write('Cantidad de actividades: '), write(Cantidad), nl, nl,
            getTopCuidades(Resto, Conta, Objetivo)
    ;
        % Si ya llego a su objetivo dejar de imprimir la información
        true
    ).

% Entrada: Lista
% Salida: Sorted (Lista ordenada de mayor a menor)
% Restricciones: Sort solo ordenada ascendentemente hay hacer que ordene descendentemente
% Objetivo: Ordena la lista de mayor a menor en cantidad.
sortedLista(Lista, Sorted) :-

    % Separa el elemento de la cantidad para poder hacer el sort 
    findall(
        [Cantidad, Elemento], 
        member([Elemento, Cantidad], Lista), 
        CantidadElementoPairs),
    sort(0, @>=, CantidadElementoPairs, SortedPairs), 

    % Invertir los elmentos
    findall(
        [Elemento, Cantidad], 
        member([Cantidad, Elemento], SortedPairs), 
        Sorted).

% Entrada: Destinos (Lista de destinos), DestinoAcum (Lista que acumula)
% Salida: DestinoFinal (Lista final cn todos los destinos)
% Restricciones: Si hay un destino con 0 actividades no se añade a la lista
% Objetivo: Crea una lista de l destino y la cantidad de actividades
cantActividades([], DestinoAcum, DestinoAcum).
cantActividades([Cabeza | Resto], DestinoAcum, DestinoFinal) :-
    findall(
        Actividad,
        asociar_actividad(Cabeza, Actividad),
        Actividades),
    length(Actividades, Tamano),
    (   Tamano \= 0 -> 
        % Si hay actividades, añadir a la lista
        cantActividades(Resto, [[Cabeza, Tamano] | DestinoAcum], DestinoFinal)
    ;   
        % Si no hay, no añadir y continuar con el resto
        cantActividades(Resto, DestinoAcum, DestinoFinal)
    ).

% Entrada: Ninguna
% Salida: Ninguna
% Restricciones: Ninguna
% Objetivo: Hace la consulta para buscar las top 3 cuidades con más actividades
topCuidades :- 
    findall(
        Destino,
        destino(Destino, _),
        Destinos),
    cantActividades(Destinos, [], DestinoFinal),
    length(DestinoFinal, Tamano),
    (   Tamano = 0 ->
        % Si es 0 dar el mensaje de que no hay actividades asociadas a los destinos todavía
        write('No hay actividades asociadas a los destinos'), nl,
        write('Asocie actividades para poder hacerlo'), nl, nl
    ;   Tamano = 1 -> 
        % Si el tamaño es 1 dar como objetivo 1 y no hacer el sorted
        getTopCuidades(DestinoFinal, 0, 1)
    ;   Tamano = 2 -> 
        % Si es 2 dar como objetivo 2
        sortedLista(DestinoFinal, SortedDestinos),
        getTopCuidades(SortedDestinos, 0, 2)
    ;   Tamano >= 3 ->
        % Si es mayor o igual a 3 el objetivo va a ser 3
        sortedLista(DestinoFinal, SortedDestinos),
        getTopCuidades(SortedDestinos, 0, 3)
    ).

% Entrada: Actividad, Lista (anterior)
% Salida: NuevaLista (resultante)
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
% Objetivo: Calcula la actividad mas cara comprando costos
encontrarMasCara([], ActividadesMasCaras, _) :-
    % Imprime todas las actividades que comparten el costo maximo
    forall(member(Actividad, ActividadesMasCaras),
           (actividad(Actividad, Costo, Duracion, Descripcion, Tipo),
            write('Nombre: '), write(Actividad),
            nl, write('Costo: '), write(Costo),
            nl, write('Duracion: '), write(Duracion), write(' dias'),
            nl, write('Descripcion: '), write(Descripcion),
            nl, write('Categorias: '), mostrarTipos(Tipo), nl, nl)).

encontrarMasCara([Cabeza | Resto], ActividadesMasCaras, CostoMax) :-
    actividad(Cabeza, Costo, _, _, _),
    (   Costo > CostoMax ->
        % Actualiza la lista solo con esta actividad si es mas cara
        encontrarMasCara(Resto, [Cabeza], Costo)
    ;   Costo =:= CostoMax ->
        % Agrega la actividad a la lista si el costo es igual al maximo actual y no esta repetida
        agregar_si_no_existe(Cabeza, ActividadesMasCaras, NuevaActividadesMasCaras),
        encontrarMasCara(Resto, NuevaActividadesMasCaras, CostoMax)
    ;   encontrarMasCara(Resto, ActividadesMasCaras, CostoMax)
    ).

% Entrada: Ninguna
% Salida: Niguna
% Restricciones: Ninguna
% Objetivo: Hace la consulta para buscar la actividad mas cara
actividadMasCara :-
    crearListaActividades(Actividades),
    encontrarMasCara(Actividades, _, 0).

% Entrada: Lista de actividades, Actividad (acumulada), DuracionMin (acumulado)
% Salida: Ninguna
% Restricciones: Si hay actividades con una misma duracion minima entonces se mostraran tambien
% Objetivo: Calcula la actividad con menos duracion comparando cantidad de dias
encontrarMenorDuracion([], ActividadesMenorDuracion, _) :-
    % Imprime todas las actividades que comparten la duracion minima
    forall(member(Actividad, ActividadesMenorDuracion),
           (actividad(Actividad, Costo, Duracion, Descripcion, Tipo),
            write('Nombre: '), write(Actividad),
            nl, write('Costo: '), write(Costo),
            nl, write('Duracion: '), write(Duracion), write(' dias'),
            nl, write('Descripcion: '), write(Descripcion),
            nl, write('Categorias: '), mostrarTipos(Tipo), nl, nl)).

encontrarMenorDuracion([Cabeza | Resto], ActividadesMenorDuracion, DuracionMin) :-
    actividad(Cabeza, _, Duracion, _, _),
    (   Duracion < DuracionMin ->
        % Actualiza la lista solo con esta actividad si es de menor duracion
        encontrarMenorDuracion(Resto, [Cabeza], Duracion)
    ;   Duracion =:= DuracionMin ->
        % Agrega la actividad a la lista si la duracion es igual a la minima actual y no esta repetida
        agregar_si_no_existe(Cabeza, ActividadesMenorDuracion, NuevaActividadesMenorDuracion),
        encontrarMenorDuracion(Resto, NuevaActividadesMenorDuracion, DuracionMin)
    ;   encontrarMenorDuracion(Resto, ActividadesMenorDuracion, DuracionMin)
    ).

% Entrada: Ninguna
% Salida: Niguna
% Restricciones: Ninguna
% Objetivo: Hace la consulta para buscar la actividad con menos duracion
actividadMenorDuracaion :-
    crearListaActividades(Actividades),
    encontrarMenorDuracion(Actividades, _, 999).


% Entrada: Lista (La lista con las categorias), Objetivo(la cantidad maxima de una categoria)
% Salida: Niguna
% Restricciones: Si hay vatrias categorias con la misma cantidad se imprimen 
% Objetivo: Imprimir la categoria/categorias con más actividades
imprimirCategoriaAct([[Categoria, Cantidad] | Cola], Objetivo) :-
    (   Cantidad =:= Objetivo ->  
            write('Categoria: '), write(Categoria), nl,
            write('Cantidad de actividades: '), write(Cantidad), nl, nl,
            imprimirCategoriaAct(Cola, Objetivo)
    ;   !
    ).

% Entrada: Lista (lista ordenada de categorias)
% Salida: Cantidad (la cantidad max de actividades en una categoria)
% Restricciones: Ninguna
% Objetivo: Sacar la cantidad más alta de actividades en una categoria
cantMax([[_, Cantidad] | _], Cantidad).

% Entrada: Elemento (categoria que se busca), CntadorAcum(contador que se va creando)
% Salida: Contador (contador resultado)
% Restricciones: La cabeza debe ser igual al elemento.
% Objetivo: Contar cuantas veces la categoria sale en la lista de duplicados
contarCategoria(_, [], ContadorAcum, ContadorAcum).
contarCategoria(Elemento, [Cabeza | Cola], ContadorAcum, Contador) :-
    (   Elemento = Cabeza ->  
            Cont is ContadorAcum + 1,
            contarCategoria(Elemento, Cola, Cont, Contador)
    ;   contarCategoria(Elemento, Cola, ContadorAcum, Contador)
    ).

% Entrada: ListaRecorrer (lista que va a ser recorrida), 
%   Lista (lista original), ListaAcum (lista que se va a ir creando)
% Salida: CateCantidad (Lista final con categoria y cantidad de actividades)
% Restricciones: Ninguna
% Objetivo: Recorrer la lista de categorias sin duplicados para contar las categorias
recorrerCategorias([], _, ListaAcum, ListaAcum).
recorrerCategorias([Cabeza | Cola], Lista, ListaAcum, CateCantidad) :-
    contarCategoria(Cabeza, Lista, 0, Cantidad),
    recorrerCategorias(Cola, Lista, [[Cabeza, Cantidad] | ListaAcum], CateCantidad).
    

% Entrada: Lista (lista a recorrer), ListaAcum(lista qeu se va acumulando)
% Salida: ListaResultado (lista que se va a dar como resultado)
% Restricciones: Ninguna
% Objetivo: Va añadiendo a la lista las categorias que estan las sublistas
agregarALista([], ListaAcum, ListaAcum).
agregarALista([Cabeza | Cola], ListaAcum, ListaResultado) :-
    agregarALista(Cola, [Cabeza | ListaAcum], ListaResultado).

% Entrada: Lista de categorias en actividades, ListaAcum(Lista acumulada)
% Salida: ListaFinal (Lista final con todas las categorias)
% Restricciones: La lista de categorias tiene sublistas
% Objetivo: Crea una sola lista con todas las categorias en las actividades, aunque se repitan
crearListaCategorias([], ListaAcum, ListaAcum).
crearListaCategorias([Cabeza | Cola], ListaAcum, ListaFinal) :-
    agregarALista(Cabeza, ListaAcum, ListaResultado),
    crearListaCategorias(Cola, ListaResultado, ListaFinal).

% Entrada: Ninguna
% Salida: Niguna
% Restricciones: Ninguna
% Objetivo: Enseña la categoria con más actividades y la cantidad de actividades en esa categoria
categoriaActividades :-
    findall(
        Categorias,
        actividad(_, _, _, _, Categorias),
        ListaCategorias),
    crearListaCategorias(ListaCategorias, [], ListaFinal),

    % Sort elimina duplicados
    sort(ListaFinal, ListaSinDuplicados),

    % Recorre la lista y crea una lista con las categorias y la cantidad de actividades
    recorrerCategorias(ListaSinDuplicados, ListaFinal, [], CateCantidad),

    % Saca la categoria con más actividades
    sortedLista(CateCantidad, ListaSort),
    cantMax(ListaSort, Cant),
    imprimirCategoriaAct(ListaSort, Cant).

% Entrada: Ninguna
% Salida: Niguna
% Restricciones: Ninguna
% Objetivo: Presentar el menu de estadisticas
menuEstadisticas :- 
    write('----- Menu Estadisticas -----'), nl,
    write('a. Top 3 ciudades con mas actividades'), nl,
    write('b. Actividad mas cara.'), nl,
    write('c. Actividad de menor duracion'), nl,
    write('d. Categoria con mas actividades'), nl,
    write('e. Volver'), nl,
    write('Ingrese la opcion que desea: '),
    read(Opcion), nl, nl,
    manejarMenuEstadisticas(Opcion).

% Entrada: Opciones validas
% Salida: Niguna
% Restricciones: Que la opcion sea valida, si no dar mensaje de error.
% Objetivo: Manejar las opciones del menu de estadisticas
manejarMenuEstadisticas(a) :- topCuidades, nl, menuEstadisticas.
manejarMenuEstadisticas(b) :- actividadMasCara, nl, menuEstadisticas.
manejarMenuEstadisticas(c) :- actividadMenorDuracaion, nl, menuEstadisticas.
manejarMenuEstadisticas(d) :- categoriaActividades, nl, menuEstadisticas.
manejarMenuEstadisticas(e) :- !, true.
manejarMenuEstadisticas(_) :- write('Opcion invalida, vuelva a intentarlo.'), nl,nl, menuEstadisticas.

% Entrada: Ninguna
% Salida: Ninguna
% Restricciones: Ninguna
% Objetivo: Enseñar las opciones del menu
menu :-
    write('----- Menu Administrativo -----'), nl,
    write('a. Agregar hechos'), nl,
    write('b. Consulta de destino'), nl,
    write('c. Actividades por tipo'), nl,
    write('d. Consulta por precio'), nl,
    write('e. Generar itinerario por monto'), nl,
    write('f. Generar itinerario por dias'), nl,
    write('g. Recomendar por frase'), nl,
    write('h. Estadisticas'), nl,
    write('j. Volver'), nl,
    write('Ingrese la opcion que desea: '),
    read(Opcion), nl, nl,
    manejarMenu(Opcion).

% Entrada: Opciones validas
% Salida: Ninguna
% Restricciones: Que la opcion dada sea valida, si no dar mensaje de error
% Objetivo: Manejar las opciones del menu
manejarMenu(a) :- menuAgregarHechos, menu.
manejarMenu(b) :- consultarDestino, menu.
manejarMenu(c) :- actividadXtipo, menu.
manejarMenu(d) :- consultaXprecio, menu.
manejarMenu(e) :- itinerarioXmonto, menu.
manejarMenu(f) :- itinerarioXdias, menu.
manejarMenu(g) :- recomendarXFrase, menu.
manejarMenu(h) :- menuEstadisticas, menu.
manejarMenu(j) :- !, true.
manejarMenu(_) :- write('Opcion invalida, vuelva a intentarlo.'), nl,nl, menu.


% Entrada: Ninguna
% Salida: Ninguna
% Restricciones: Ninguna
% Objetivo: Enseña las opciones del menu principal 
menuPrincipal :-
    write('----- Menu Principal -----'), nl,
    write('m. Menu Admiistrativo'), nl,
    write('s. Salir'), nl,
    write('Ingrese la opcion que desea: '),
    read(Opcion), nl, nl,
    manejarMenuPrincipal(Opcion).

% Entrada: Opciones validas
% Salida: Ninguna
% Restricciones: Si la opcion no es valida dar un mensaje de error
% Objetivo: Manejar las opciones del menu principal
manejarMenuPrincipal(m) :- menu, menuPrincipal.
manejarMenuPrincipal(s) :- write('Saliendo del programa...'), nl, true.
manejarMenuPrincipal(_) :- write('Opcion invalida, vuelva a intentarlo.'), nl, nl, menuPrincipal.


% Entrada: Ninguna
% Salida: Ninguna
% Restricciones: Ninguna
% Objetivo: iniciar el programa 
iniciar :- menuPrincipal.
