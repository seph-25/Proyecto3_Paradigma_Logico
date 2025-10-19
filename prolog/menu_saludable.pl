% ============================================================================
% SISTEMA DE MENÚS SALUDABLES
% ============================================================================

:- dynamic historial_menu/4.

% ============================================================================
% BASE DE DATOS DE ALIMENTOS
% ============================================================================

% entrada(Nombre, Calorias, Vegetariano)
entrada('Ensalada César', 150, false).
entrada('Ensalada Verde', 80, true).
entrada('Sopa de Verduras', 120, true).
entrada('Sopa de Pollo', 180, false).
entrada('Carpaccio de Res', 200, false).
entrada('Bruschetta', 140, true).
entrada('Gazpacho', 100, true).
entrada('Ceviche', 160, false).

% carbohidrato(Nombre, Calorias, Vegetariano)
carbohidrato('Arroz Integral', 215, true).
carbohidrato('Arroz Blanco', 205, true).
carbohidrato('Pasta Integral', 180, true).
carbohidrato('Quinoa', 120, true).
carbohidrato('Puré de Papa', 170, true).
carbohidrato('Batata Asada', 160, true).
carbohidrato('Pan Integral', 140, true).

% carne(Nombre, Calorias, Tipo, Vegetariano)
carne('Pollo a la Plancha', 165, pollo, false).
carne('Pollo al Horno', 190, pollo, false).
carne('Res a la Plancha', 250, res, false).
carne('Lomo de Res', 280, res, false).
carne('Pescado al Vapor', 150, pescado, false).
carne('Salmón a la Plancha', 206, pescado, false).
carne('Atún Sellado', 184, pescado, false).
carne('Cerdo Agridulce', 230, cerdo, false).
carne('Tofu Salteado', 145, vegetariano, true).
carne('Lentejas Guisadas', 230, vegetariano, true).
carne('Garbanzos al Curry', 210, vegetariano, true).

% vegetal(Nombre, Calorias, Vegetariano)
vegetal('Brócoli al Vapor', 55, true).
vegetal('Espinacas Salteadas', 40, true).
vegetal('Zanahorias Glaseadas', 80, true).
vegetal('Judías Verdes', 44, true).
vegetal('Calabacín a la Plancha', 33, true).
vegetal('Champiñones Salteados', 28, true).
vegetal('Espárragos', 27, true).
vegetal('Col Rizada', 49, true).

% postre(Nombre, Calorias, Vegetariano)
postre('Fruta Fresca', 60, true).
postre('Yogur Natural', 100, true).
postre('Gelatina', 80, true).
postre('Mousse de Chocolate', 150, true).
postre('Flan', 140, true).
postre('Helado de Vainilla', 137, true).
postre('Tarta de Manzana', 180, true).

% ============================================================================
% PREDICADOS DE FILTRADO
% ============================================================================

% Filtrar por vegetariano
cumple_vegetariano(_, false).
cumple_vegetariano(true, true).

% Filtrar tipo de carne
cumple_tipo_carne(_, todas).
cumple_tipo_carne(Tipo, Tipo).

% Filtrar por rango de calorías
cumple_calorias(Calorias, Min, Max) :-
    (Min = none ; Calorias >= Min),
    (Max = none ; Calorias =< Max).

% ============================================================================
% GENERACIÓN DE MENÚS
% ============================================================================

% menu_valido(Entrada, Carb, Carne, Vegetal, Postre, CalTotal, Filtros)
menu_valido(EntradaN, CarbN, CarneN, VegN, PostreN, CalTotal,
            es_veg(EsVeg), tipo_carne(TipoCarne), con_postre(ConPostre),
            cal_min(CalMin), cal_max(CalMax)) :-
    % Entrada
    entrada(EntradaN, CalE, VegE),
    cumple_vegetariano(VegE, EsVeg),

    % Carbohidrato
    carbohidrato(CarbN, CalC, VegC),
    cumple_vegetariano(VegC, EsVeg),

    % Carne
    carne(CarneN, CalCarne, TipoCarnePlato, VegCarne),
    cumple_vegetariano(VegCarne, EsVeg),
    cumple_tipo_carne(TipoCarnePlato, TipoCarne),

    % Vegetal
    vegetal(VegN, CalV, VegVeg),
    cumple_vegetariano(VegVeg, EsVeg),

    % Postre
    (ConPostre = true ->
        postre(PostreN, CalP, VegP),
        cumple_vegetariano(VegP, EsVeg)
    ;
        PostreN = ninguno,
        CalP = 0
    ),

    % Calcular calorías totales
    CalTotal is CalE + CalC + CalCarne + CalV + CalP,

    % Verificar rango de calorías
    cumple_calorias(CalTotal, CalMin, CalMax).

% Generar N menús únicos
generar_menus(Cantidad, Filtros, Menus) :-
    findall(
        menu(EntradaN, CarbN, CarneN, VegN, PostreN, CalTotal),
        menu_valido(EntradaN, CarbN, CarneN, VegN, PostreN, CalTotal,
                    es_veg(EsVeg), tipo_carne(TipoCarne), con_postre(ConPostre),
                    cal_min(CalMin), cal_max(CalMax)),
        TodosMenus
    ),
    extraer_filtros(Filtros, EsVeg, TipoCarne, ConPostre, CalMin, CalMax),
    seleccionar_diversos(TodosMenus, Cantidad, Menus).

% Extraer valores de filtros
extraer_filtros([es_veg(V), tipo_carne(T), con_postre(P), cal_min(Min), cal_max(Max)],
                V, T, P, Min, Max).

% Seleccionar menús diversos (simplificado)
seleccionar_diversos(Menus, Cantidad, Seleccionados) :-
    length(Menus, Total),
    (Total =< Cantidad ->
        Seleccionados = Menus
    ;
        random_permutation(Menus, Shuffled),
        length(Seleccionados, Cantidad),
        append(Seleccionados, _, Shuffled)
    ).

% ============================================================================
% HISTORIAL Y APRENDIZAJE
% ============================================================================

% Registrar aceptación/rechazo
registrar_accion(Accion, Menu, Filtros, Timestamp) :-
    assertz(historial_menu(Accion, Menu, Filtros, Timestamp)).

% Obtener estadísticas
estadisticas(Aceptados, Rechazados, Total) :-
    findall(1, historial_menu(aceptado, _, _, _), ListaAceptados),
    findall(1, historial_menu(rechazado, _, _, _), ListaRechazados),
    length(ListaAceptados, Aceptados),
    length(ListaRechazados, Rechazados),
    Total is Aceptados + Rechazados.

% Obtener menús más aceptados
menus_favoritos(Menus) :-
    findall(Menu, historial_menu(aceptado, Menu, _, _), Menus).

% ============================================================================
% UTILIDADES
% ============================================================================

% Limpiar historial
limpiar_historial :-
    retractall(historial_menu(_, _, _, _)).

% Contar combinaciones posibles
contar_combinaciones(Filtros, Total) :-
    findall(1, menu_valido(_, _, _, _, _, _,
                          es_veg(EsVeg), tipo_carne(TipoCarne),
                          con_postre(ConPostre), cal_min(CalMin), cal_max(CalMax)),
            Lista),
    extraer_filtros(Filtros, EsVeg, TipoCarne, ConPostre, CalMin, CalMax),
    length(Lista, Total).