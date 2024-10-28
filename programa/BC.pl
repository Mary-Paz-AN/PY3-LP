:- dynamic destino/2.
:- discontiguous destino/2.
:- dynamic actividad/5.
:- discontiguous actividad/5.
:- dynamic asociar_actividad/2.
:- discontiguous asociar_actividad/2.
:- dynamic datos_itinerario/4.

destino(paris, 'Ciudad de la Luz').
destino(nueva_york, 'La Gran Manzana').
actividad(museo_louvre, 25, 2, 'Visitar el Museo del Louvre', ['arte', 'historia']).
actividad(paseo_en_bici, 30, 3, 'Paseo en bicicleta por la ciudad', ['aventura', 'naturaleza']).
actividad(paseo_en_carro, 30, 3, 'Paseo en carro', ['aventura', 'naturaleza']).
asociar_actividad(paris, museo_louvre).
asociar_actividad(paris, paseo_en_carro).
asociar_actividad(nueva_york, paseo_en_bici).
destino(tokyo, 'Cuidad de la Tecnologia').
destino(seul, 'Cuidad del Skincare').
destino(limon, 'Cuidad de las mejores playas').
actividad(playa, 100, 4, 'Perfecta para relajarse', ['aventura', 'diversion']).
actividad(concierto, 2000, 2, 'Cantar hasta no poder', ['experiencia', 'diversion']).
asociar_actividad(tokyo, concierto).
asociar_actividad(paris, concierto).
asociar_actividad(limon, playa).
actividad(karaoke, 500, 3, 'Para pasar el rato', ['experiencia', 'diversion']).
actividad(ver_pajaros, 120, 2, 'Pasar una tarde relajante', ['educacion', 'naturaleza', 'panorama', 'cultura']).
destino(guanacaste, 'Lindo para ver vacas y extraordinarias playas').
actividad(carreras_de_autos, 560, 4, 'Adrenalina al full', ['aventura', 'experiencia', 'diversion']).
asociar_actividad(guanacaste, carreras_de_autos).
