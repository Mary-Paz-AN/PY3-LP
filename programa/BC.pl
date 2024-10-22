% se agregan destinos, actividades y asociar_actividad para que la base de datos tenga algunoos al iniciar el programa %

% destino/2 %
destino(paris, 'Ciudad de la Luz').
destino(nueva_york, 'La Gran Manzana').
destino(atomo, descripcion).

% actividad/5 %
actividad(museo_louvre, 25, 2, 'Visitar el Museo del Louvre', ['arte', 'historia']).
actividad(paseo_en_bici, 30, 3, 'Paseo en bicicleta por la ciudad', ['aventura', 'naturaleza']).
actividad(atomo, costo, duracion, descripcion, lista_tipo).

% asociar_actividad/2 %
asociar_actividad(paris, museo_louvre).
asociar_actividad(nueva_york, paseo_en_bici).
asociar_actividad(destino, actividad).