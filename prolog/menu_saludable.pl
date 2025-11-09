% ============================================================================
% DYNAMIC PREDICATES FOR LEARNING
% ============================================================================

:- dynamic(regla_preferencia/3).
:- dynamic(regla_aversion/3).

% regla_preferencia(Componente, Valor, Razon).
% regla_aversion(Componente, Valor, Razon).

% ============================================================================
% KNOWLEDGE BASE: FOOD DATABASE
% ============================================================================

% entrada(Name, Calories, Vegetarian)
entrada('Ensalada Cesar', 150, false).
entrada('Ensalada Verde', 80, true).
entrada('Sopa de Verduras', 120, true).
entrada('Sopa de Pollo', 180, false).
entrada('Carpaccio de Res', 200, false).
entrada('Bruschetta', 140, true).
entrada('Gazpacho', 100, true).
entrada('Ensalada rusa', 190, false).
entrada('Ensalada de caracoles', 200, false).

% carbohidrato(Name, Calories, Vegetarian)
carbohidrato('Arroz Integral', 215, true).
carbohidrato('Arroz Blanco', 205, true).
carbohidrato('Pasta Integral', 180, true).
carbohidrato('Quinoa', 120, true).
carbohidrato('Pure de Papa', 170, true).
carbohidrato('Batata Asada', 160, true).
carbohidrato('Pan Integral', 140, true).

% carne(Name, Calories, Type, Vegetarian)
carne('Pollo a la Plancha', 165, pollo, false).
carne('Pollo al Horno', 190, pollo, false).
carne('Res a la Plancha', 250, res, false).
carne('Lomo de Res', 280, res, false).
carne('Pescado al Vapor', 150, pescado, false).
carne('Salmon a la Plancha', 206, pescado, false).
carne('Atun Sellado', 184, pescado, false).
carne('Cerdo Agridulce', 230, cerdo, false).
carne('Tofu Salteado', 145, vegetariano, true).
carne('Lentejas Guisadas', 230, vegetariano, true).
carne('Garbanzos al Curry', 210, vegetariano, true).

% vegetal(Name, Calories, Vegetarian)
vegetal('Brocoli al Vapor', 55, true).
vegetal('Espinacas Salteadas', 40, true).
vegetal('Zanahorias Glaseadas', 80, true).
vegetal('Judias Verdes', 44, true).
vegetal('Calabacin a la Plancha', 33, true).
vegetal('Champinones Salteados', 28, true).
vegetal('Esparragos', 27, true).
vegetal('Col Rizada', 49, true).

% postre(Name, Calories, Vegetarian)
postre('Fruta Fresca', 60, true).
postre('Yogur Natural', 100, true).
postre('Gelatina', 80, true).
postre('Mousse de Chocolate', 150, true).
postre('Flan', 140, true).
postre('Helado de Vainilla', 137, true).
postre('Tarta de Manzana', 180, true).

% ============================================================================
% FILTERING PREDICATES
% ============================================================================

% Filter by vegetarian preference
filter_vegetarian(true, _, _, Vegetarian) :-
    Vegetarian = true.
filter_vegetarian(false, _, _, _).

% Filter meat by type
filter_meat_type(todas, _, _, _, _).
filter_meat_type(0, _, _, _, _).  % 0 means "todas" - accept all meat types
filter_meat_type(MeatType, _, _, Type, _) :-
    MeatType \= todas,
    MeatType \= 0,
    MeatType = Type.

% Filter ingredients to include specific ones - INCLUSIVO CON COINCIDENCIA INTELIGENTE
% filter_ingredientes_incluir(+IngredientName, +NombreItem, +ListaIncluir, +Result)
% Si la lista está vacía, incluir TODO (comportamiento por defecto)
filter_ingredientes_incluir(_, _, [], true).
% Si la lista no está vacía, siempre incluir (comportamiento inclusivo por defecto)
filter_ingredientes_incluir(_, _, ListaIncluir, true) :-
    ListaIncluir \= [].

% Helper para coincidencias case-insensitive y parciales (para uso futuro)
ingrediente_coincide(Ingrediente, Lista) :-
    member(Item, Lista),
    downcase_atom(Ingrediente, IngLower),
    downcase_atom(Item, ItemLower),
    (sub_atom(IngLower, _, _, _, ItemLower) ; 
     sub_atom(ItemLower, _, _, _, IngLower)).

% Filter ingredients to exclude specific ones - CON COINCIDENCIAS INTELIGENTES
% filter_ingredientes_excluir(+IngredientName, +NombreItem, +ListaExcluir, +Result)
filter_ingredientes_excluir(_, _, [], true).
filter_ingredientes_excluir(IngredientName, NombreItem, ListaExcluir, false) :-
    ListaExcluir \= [],
    (ingrediente_coincide(IngredientName, ListaExcluir) ; 
     ingrediente_coincide(NombreItem, ListaExcluir)).
filter_ingredientes_excluir(IngredientName, NombreItem, ListaExcluir, true) :-
    ListaExcluir \= [],
    \+ ingrediente_coincide(IngredientName, ListaExcluir),
    \+ ingrediente_coincide(NombreItem, ListaExcluir).

% ============================================================================
% INGREDIENT VALIDATION
% ============================================================================

% Check if an ingredient exists in any category (with smart matching)
ingredient_exists(IngredientName) :-
    (entrada(ExistingName, _, _) ; 
     carbohidrato(ExistingName, _, _) ; 
     carne(ExistingName, _, _, _) ; 
     vegetal(ExistingName, _, _) ; 
     postre(ExistingName, _, _)),
    ingrediente_coincide(ExistingName, [IngredientName]).

% Validate all ingredients in a list exist
validate_ingredients_list([]).
validate_ingredients_list([H|T]) :-
    ingredient_exists(H),
    validate_ingredients_list(T).

% ============================================================================
% CALORIE VALIDATION
% ============================================================================

% Validate that calories are within range
% validate_calories(TotalCalories, MinCalories, MaxCalories)
validate_calories(_, none, none).
validate_calories(Total, none, Max) :-
    Max \= none,
    Total =< Max.
validate_calories(Total, Min, none) :-
    Min \= none,
    Total >= Min.
validate_calories(Total, Min, Max) :-
    Min \= none,
    Max \= none,
    Total >= Min,
    Total =< Max.

% ============================================================================
% MENU GENERATION
% ============================================================================

% Calculate total calories for a menu
% menu_calories(Entrada, Carbohidrato, Carne, Vegetal, Postre, Total)
menu_calories([_, EntradaCal, _], [_, CarbCal, _], [_, CarneCal, _, _],
              [_, VegCal, _], none, Total) :-
    Total is EntradaCal + CarbCal + CarneCal + VegCal.

menu_calories([_, EntradaCal, _], [_, CarbCal, _], [_, CarneCal, _, _],
              [_, VegCal, _], [_, PostreCal, _], Total) :-
    Total is EntradaCal + CarbCal + CarneCal + VegCal + PostreCal.

% Generate a valid menu combination (backward compatibility)
% generate_menu(VegetarianFilter, MeatTypeFilter, ConPostre, MinCal, MaxCal, Menu)
generate_menu(VegetarianFilter, MeatTypeFilter, ConPostre, MinCal, MaxCal, Menu) :-
    generate_menu_with_ingredients(VegetarianFilter, MeatTypeFilter, ConPostre, MinCal, MaxCal, [], [], Menu).

% Main predicate for compatibility - menu_saludable/6
% menu_saludable(VegetarianFilter, MeatTypeFilter, ConPostre, MinCal, MaxCal, Menu)
menu_saludable(VegetarianFilter, MeatTypeFilter, ConPostre, MinCal, MaxCal, Menu) :-
    generate_menu(VegetarianFilter, MeatTypeFilter, ConPostre, MinCal, MaxCal, Menu).

% Generate a valid menu combination with ingredient filtering
% generate_menu_with_ingredients(VegetarianFilter, MeatTypeFilter, ConPostre, MinCal, MaxCal, IncludeList, ExcludeList, Menu)
generate_menu_with_ingredients(VegetarianFilter, MeatTypeFilter, ConPostre, MinCal, MaxCal, IncludeList, ExcludeList,
              menu(Entrada, Carbohidrato, Carne, Vegetal, Postre, TotalCalorias)) :-
    % Validate ingredient lists first
    validate_ingredients_list(IncludeList),
    validate_ingredients_list(ExcludeList),
    
    % Get filtered items (SIMPLIFICADO para evitar ciclos)
    entrada(EntradaName, EntradaCal, EntradaVeg),
    filter_vegetarian(VegetarianFilter, EntradaName, EntradaCal, EntradaVeg),
    filter_ingredientes_incluir(EntradaName, EntradaName, IncludeList, IncludeEntrada),
    filter_ingredientes_excluir(EntradaName, EntradaName, ExcludeList, ExcludeEntrada),
    IncludeEntrada = true, ExcludeEntrada = true,
    Entrada = [EntradaName, EntradaCal, EntradaVeg],

    carbohidrato(CarbName, CarbCal, CarbVeg),
    filter_vegetarian(VegetarianFilter, CarbName, CarbCal, CarbVeg),
    filter_ingredientes_incluir(CarbName, CarbName, IncludeList, IncludeCarb),
    filter_ingredientes_excluir(CarbName, CarbName, ExcludeList, ExcludeCarb),
    IncludeCarb = true, ExcludeCarb = true,
    Carbohidrato = [CarbName, CarbCal, CarbVeg],

    carne(CarneName, CarneCal, CarneType, CarneVeg),
    filter_vegetarian(VegetarianFilter, CarneName, CarneCal, CarneVeg),
    filter_meat_type(MeatTypeFilter, CarneName, CarneCal, CarneType, CarneVeg),
    filter_ingredientes_incluir(CarneName, CarneName, IncludeList, IncludeCarne),
    filter_ingredientes_excluir(CarneName, CarneName, ExcludeList, ExcludeCarne),
    IncludeCarne = true, ExcludeCarne = true,
    Carne = [CarneName, CarneCal, CarneType, CarneVeg],

    vegetal(VegName, VegCal, VegVeg),
    filter_vegetarian(VegetarianFilter, VegName, VegCal, VegVeg),
    filter_ingredientes_incluir(VegName, VegName, IncludeList, IncludeVeg),
    filter_ingredientes_excluir(VegName, VegName, ExcludeList, ExcludeVeg),
    IncludeVeg = true, ExcludeVeg = true,
    Vegetal = [VegName, VegCal, VegVeg],

    % Handle postre based on ConPostre flag
    (ConPostre = true ->
        (postre(PostreName, PostreCal, PostreVeg),
         filter_vegetarian(VegetarianFilter, PostreName, PostreCal, PostreVeg),
         filter_ingredientes_incluir(PostreName, PostreName, IncludeList, IncludePostre),
         filter_ingredientes_excluir(PostreName, PostreName, ExcludeList, ExcludePostre),
         IncludePostre = true, ExcludePostre = true,
         Postre = [PostreName, PostreCal, PostreVeg])
    ;
        Postre = none
    ),

    % Calculate and validate calories
    menu_calories(Entrada, Carbohidrato, Carne, Vegetal, Postre, TotalCalorias),
    validate_calories(TotalCalorias, MinCal, MaxCal).

% Generate all valid menus (limited to avoid infinite loops)
generate_all_menus(VegetarianFilter, MeatTypeFilter, ConPostre, MinCal, MaxCal, Menus) :-
    findall(Menu,
            generate_menu(VegetarianFilter, MeatTypeFilter, ConPostre, MinCal, MaxCal, Menu),
            AllMenus),
    Menus = AllMenus.

% ============================================================================
% ALGORITMO DE DIVERSIDAD MEJORADO
% ============================================================================

% Extraer componentes individuales de un menú para análisis detallado
extract_menu_components_for_diversity(menu([E, _, _], [C, _, _], [M, _, _, _], [V, _, _], P, _), E, C, M, V, PostreNom) :-
    (P = none -> PostreNom = none ; P = [PostreNom, _, _]).

% Calcular diversidad entre dos menús (0-5, donde 5 es completamente diferente)
calculate_diversity_score(Menu1, Menu2, Score) :-
    extract_menu_components_for_diversity(Menu1, E1, C1, M1, V1, P1),
    extract_menu_components_for_diversity(Menu2, E2, C2, M2, V2, P2),
    
    % Contar diferencias en cada componente
    (E1 \= E2 -> DiffE = 1 ; DiffE = 0),
    (C1 \= C2 -> DiffC = 1 ; DiffC = 0),
    (M1 \= M2 -> DiffM = 1 ; DiffM = 0),
    (V1 \= V2 -> DiffV = 1 ; DiffV = 0),
    (P1 \= P2 -> DiffP = 1 ; DiffP = 0),
    
    Score is DiffE + DiffC + DiffM + DiffV + DiffP.

% Verificar si un menú es suficientemente diverso respecto a una lista
is_diverse_enough(_, [], true).
is_diverse_enough(Menu, [FirstMenu|RestMenus], Result) :-
    calculate_diversity_score(Menu, FirstMenu, Score),
    (Score >= 3 ->  % Requiere al menos 3 componentes diferentes
        is_diverse_enough(Menu, RestMenus, Result)
    ;
        Result = false
    ).

% NUEVA ESTRATEGIA EFICIENTE: Seleccionar N ingredientes únicos por categoría, luego combinar
select_diverse_menus_improved(_, MaxMenus, DiverseMenus) :-
    % Usar la nueva estrategia: seleccionar ingredientes únicos por categoría
    generate_diverse_menus_by_categories(MaxMenus, DiverseMenus).

% NUEVA ESTRATEGIA EFICIENTE: Seleccionar N ingredientes por categoría, luego combinar
generate_diverse_menus_by_categories(MaxMenus, DiverseMenus) :-
    % Paso 1: Obtener N ingredientes únicos de cada categoría
    select_unique_ingredients_per_category(MaxMenus, SelectedEntradas, SelectedCarbs, SelectedCarnes, SelectedVegetales, SelectedPostres),
    
    % Paso 2: Combinar ingredientes para crear menús diversos
    combine_ingredients_to_menus(MaxMenus, SelectedEntradas, SelectedCarbs, SelectedCarnes, SelectedVegetales, SelectedPostres, DiverseMenus).

% Seleccionar N ingredientes únicos de cada categoría
select_unique_ingredients_per_category(NumMenus, SelectedEntradas, SelectedCarbs, SelectedCarnes, SelectedVegetales, SelectedPostres) :-
    % Obtener todos los ingredientes disponibles
    findall([Name, Cal, Veg], entrada(Name, Cal, Veg), AllEntradas),
    findall([Name, Cal, Veg], carbohidrato(Name, Cal, Veg), AllCarbs),
    findall([Name, Cal, Type, Veg], carne(Name, Cal, Type, Veg), AllCarnes),
    findall([Name, Cal, Veg], vegetal(Name, Cal, Veg), AllVegetales),
    findall([Name, Cal, Veg], postre(Name, Cal, Veg), AllPostres),
    
    % Seleccionar hasta NumMenus ingredientes de cada categoría (sin repetir)
    select_n_unique(AllEntradas, NumMenus, SelectedEntradas),
    select_n_unique(AllCarbs, NumMenus, SelectedCarbs),
    select_n_unique(AllCarnes, NumMenus, SelectedCarnes),
    select_n_unique(AllVegetales, NumMenus, SelectedVegetales),
    select_n_unique(AllPostres, NumMenus, SelectedPostres).

% Seleccionar N elementos únicos de una lista
select_n_unique(List, N, Selected) :-
    length(List, Len),
    (Len =< N ->
        Selected = List  % Si hay menos o igual elementos, tomar todos
    ;
        random_permutation(List, Shuffled),  % Mezclar aleatoriamente
        take_first_n(Shuffled, N, Selected)  % Tomar los primeros N
    ).

% Combinar ingredientes seleccionados para crear menús
combine_ingredients_to_menus(NumMenus, Entradas, Carbs, Carnes, Vegetales, Postres, Menus) :-
    create_menu_combinations(NumMenus, Entradas, Carbs, Carnes, Vegetales, Postres, 0, [], Menus).

% Crear combinaciones de menús distribuyendo los ingredientes
create_menu_combinations(NumMenus, _, _, _, _, _, NumMenus, Menus, Menus) :- !.
create_menu_combinations(NumMenus, Entradas, Carbs, Carnes, Vegetales, Postres, Index, Acc, FinalMenus) :-
    Index < NumMenus,
    
    % Seleccionar ingredientes para este menú (rotando por las listas)
    select_ingredient_by_index(Entradas, Index, SelectedEntrada),
    select_ingredient_by_index(Carbs, Index, SelectedCarb),
    select_ingredient_by_index(Carnes, Index, SelectedCarne),
    select_ingredient_by_index(Vegetales, Index, SelectedVegetal),
    select_ingredient_by_index(Postres, Index, SelectedPostre),
    
    % Crear menú y calcular calorías
    create_menu_from_ingredients(SelectedEntrada, SelectedCarb, SelectedCarne, SelectedVegetal, SelectedPostre, Menu),
    
    NewIndex is Index + 1,
    create_menu_combinations(NumMenus, Entradas, Carbs, Carnes, Vegetales, Postres, NewIndex, [Menu|Acc], FinalMenus).

% Seleccionar ingrediente por índice (con rotación circular)
select_ingredient_by_index(List, Index, Selected) :-
    length(List, Len),
    ActualIndex is Index mod Len,
    nth0(ActualIndex, List, Selected).

% Crear menú a partir de ingredientes seleccionados
create_menu_from_ingredients(Entrada, Carb, Carne, Vegetal, Postre, Menu) :-
    menu_calories(Entrada, Carb, Carne, Vegetal, Postre, TotalCalorias),
    Menu = menu(Entrada, Carb, Carne, Vegetal, Postre, TotalCalorias).

% FUNCIONES AUXILIARES PARA LA NUEVA ESTRATEGIA EFICIENTE

% Tomar los primeros N elementos de una lista
take_first_n(_, 0, []) :- !.
take_first_n([], _, []) :- !.
take_first_n([H|T], N, [H|Result]) :-
    N > 0,
    N1 is N - 1,
    take_first_n(T, N1, Result).

% Shuffle una lista para mayor aleatorización (implementación simple)
shuffle_list(List, ShuffledList) :-
    add_random_keys(List, KeyedList),
    keysort(KeyedList, SortedList),
    remove_keys(SortedList, ShuffledList).

add_random_keys([], []).
add_random_keys([H|T], [Key-H|KeyedT]) :-
    get_time(Time),
    Key is round(Time * 1000000) mod 1000000,  % Usar tiempo como semilla pseudo-aleatoria
    add_random_keys(T, KeyedT).

remove_keys([], []).
remove_keys([_Key-Value|T], [Value|ResultT]) :-
    remove_keys(T, ResultT).

% Predicado principal mejorado para obtener menús diversos
get_diverse_menus_improved(VegetarianFilter, MeatTypeFilter, ConPostre, MinCal, MaxCal, MaxMenus, DiverseMenus) :-
    % Generar todos los menús posibles
    findnsols(200, Menu,  % Limitar a 200 para mejor rendimiento
              generate_menu_with_ingredients(VegetarianFilter, MeatTypeFilter, ConPostre, MinCal, MaxCal, [], [], Menu),
              AllMenus),
    
    % Aleatorizar la lista inicial para evitar patrones
    shuffle_list(AllMenus, ShuffledMenus),
    
    % Seleccionar menús diversos
    select_diverse_menus_improved(ShuffledMenus, MaxMenus, DiverseMenus).

% Predicate to extract menu components for easier Python integration
extract_menu_components(menu([EntradaNom, EntradaCal, EntradaVeg],
                             [CarbNom, CarbCal, CarbVeg],
                             [CarneNom, CarneCal, CarneTipo, CarneVeg],
                             [VegNom, VegCal, VegVeg],
                             Postre,
                             TotalCal),
                        EntradaNom, EntradaCal, EntradaVeg,
                        CarbNom, CarbCal, CarbVeg,
                        CarneNom, CarneCal, CarneTipo, CarneVeg,
                        VegNom, VegCal, VegVeg,
                        PostreNom, PostreCal, PostreVeg,
                        TotalCal) :-
    (Postre = none ->
        PostreNom = none, PostreCal = 0, PostreVeg = true
    ;
        Postre = [PostreNom, PostreCal, PostreVeg]
    ).

% Helper predicate to get a menu with all components extracted (backward compatibility)
get_menu_details(VegetarianFilter, MeatTypeFilter, ConPostre, MinCal, MaxCal,
                 EntradaNom, EntradaCal, EntradaVeg,
                 CarbNom, CarbCal, CarbVeg,
                 CarneNom, CarneCal, CarneTipo, CarneVeg,
                 VegNom, VegCal, VegVeg,
                 PostreNom, PostreCal, PostreVeg,
                 TotalCal) :-
    get_menu_details_with_ingredients(VegetarianFilter, MeatTypeFilter, ConPostre, MinCal, MaxCal, [], [],
                                    EntradaNom, EntradaCal, EntradaVeg,
                                    CarbNom, CarbCal, CarbVeg,
                                    CarneNom, CarneCal, CarneTipo, CarneVeg,
                                    VegNom, VegCal, VegVeg,
                                    PostreNom, PostreCal, PostreVeg,
                                    TotalCal).

% Helper predicate to get a menu with all components extracted including ingredient filtering
get_menu_details_with_ingredients(VegetarianFilter, MeatTypeFilter, ConPostre, MinCal, MaxCal, IncludeList, ExcludeList,
                                 EntradaNom, EntradaCal, EntradaVeg,
                                 CarbNom, CarbCal, CarbVeg,
                                 CarneNom, CarneCal, CarneTipo, CarneVeg,
                                 VegNom, VegCal, VegVeg,
                                 PostreNom, PostreCal, PostreVeg,
                                 TotalCal) :-
    generate_menu_with_ingredients(VegetarianFilter, MeatTypeFilter, ConPostre, MinCal, MaxCal, IncludeList, ExcludeList, Menu),
    extract_menu_components(Menu,
                           EntradaNom, EntradaCal, EntradaVeg,
                           CarbNom, CarbCal, CarbVeg,
                           CarneNom, CarneCal, CarneTipo, CarneVeg,
                           VegNom, VegCal, VegVeg,
                           PostreNom, PostreCal, PostreVeg,
                           TotalCal).

% ============================================================================
% LEARNING SYSTEM: SYMBOLIC INDUCTION
% ============================================================================

% Register acceptance of menu components
% registrar_aceptacion(+Componente, +Valor, +Razon)
registrar_aceptacion(Componente, Valor, Razon) :-
    assertz(regla_preferencia(Componente, Valor, Razon)).

% Register rejection of menu components  
% registrar_rechazo(+Componente, +Valor, +Razon)
registrar_rechazo(Componente, Valor, Razon) :-
    assertz(regla_aversion(Componente, Valor, Razon)).

% Count occurrences of a specific component value in preferences
count_preferencias(Componente, Valor, Count) :-
    findall(1, regla_preferencia(Componente, Valor, _), List),
    length(List, Count).

% Count occurrences of a specific component value in aversions
count_aversiones(Componente, Valor, Count) :-
    findall(1, regla_aversion(Componente, Valor, _), List),
    length(List, Count).

% Count total occurrences (preferences + aversions)
count_total_ocurrencias(Componente, Valor, Total) :-
    count_preferencias(Componente, Valor, Prefs),
    count_aversiones(Componente, Valor, Avers),
    Total is Prefs + Avers.

% Calculate confidence of a rule based on acceptance rate
% calcular_confianza_regla(+Componente, +Valor, -Confianza)
calcular_confianza_regla(Componente, Valor, Confianza) :-
    count_preferencias(Componente, Valor, Aceptaciones),
    count_total_ocurrencias(Componente, Valor, Total),
    (Total > 0 -> 
        Confianza is Aceptaciones / Total
    ; 
        Confianza is 0
    ).

% Check if an element is preferred based on confidence threshold
es_preferido(Componente, Valor) :-
    calcular_confianza_regla(Componente, Valor, Confianza),
    Confianza >= 0.7.  % 70% threshold

% Check if an element is disliked based on confidence threshold  
es_rechazado(Componente, Valor) :-
    count_aversiones(Componente, Valor, Aversiones),
    count_total_ocurrencias(Componente, Valor, Total),
    Total > 0,
    ConfianzaRechazo is Aversiones / Total,
    ConfianzaRechazo >= 0.7.  % 70% threshold

% Calculate score for a menu component based on learned rules
% calcular_puntuacion_componente(+Componente, +Valor, -Puntuacion) - Safe version with confidence
calcular_puntuacion_componente(Componente, Valor, Puntuacion) :-
    % Calculate preferences safely
    findall(_, regla_preferencia(Componente, Valor, _), Preferencias),
    length(Preferencias, NumPreferencias),
    
    % Calculate aversions safely  
    findall(_, regla_aversion(Componente, Valor, _), Aversiones),
    length(Aversiones, NumAversiones),
    
    % Simple but effective scoring
    (NumPreferencias > 0, NumAversiones =:= 0 ->
        Puntuacion is NumPreferencias * 20
    ; NumAversiones > 0, NumPreferencias =:= 0 ->
        Puntuacion is -(NumAversiones * 20)
    ; NumPreferencias > NumAversiones ->
        Puntuacion is (NumPreferencias - NumAversiones) * 10
    ; NumAversiones > NumPreferencias ->
        Puntuacion is -((NumAversiones - NumPreferencias) * 10)
    ; 
        Puntuacion is 0
    ).

% Calculate total score for a menu based on learned preferences
% aplicar_reglas_aprendidas(+Menu, -Puntuacion)
aplicar_reglas_aprendidas(menu([EntradaNom, _, _], [CarbNom, _, _], [CarneNom, _, _, _], 
                               [VegNom, _, _], Postre, _), PuntuacionTotal) :-
    calcular_puntuacion_componente(entrada, EntradaNom, PuntEntrada),
    calcular_puntuacion_componente(carbohidrato, CarbNom, PuntCarb),
    calcular_puntuacion_componente(carne, CarneNom, PuntCarne),
    calcular_puntuacion_componente(vegetal, VegNom, PuntVeg),
    
    (Postre = none ->
        PuntPostre = 0
    ; 
        Postre = [PostreNom, _, _],
        calcular_puntuacion_componente(postre, PostreNom, PuntPostre)
    ),
    
    PuntuacionTotal is PuntEntrada + PuntCarb + PuntCarne + PuntVeg + PuntPostre.

% Helper predicate to create menu-score pairs
menu_con_puntuacion(Menu, menu_puntuado(Menu, Puntuacion)) :-
    aplicar_reglas_aprendidas(Menu, Puntuacion).

% Sort menus by learned preferences (highest score first)
% priorizar_menus(+ListaMenus, -MenusOrdenados)
priorizar_menus(ListaMenus, MenusOrdenados) :-
    maplist(menu_con_puntuacion, ListaMenus, MenusPuntuados),
    predsort(comparar_puntuaciones, MenusPuntuados, MenusOrdenadosPunt),
    maplist(extraer_menu, MenusOrdenadosPunt, MenusOrdenados).

% Compare two scored menus (for sorting - descending order)
comparar_puntuaciones(Orden, menu_puntuado(_, Punt1), menu_puntuado(_, Punt2)) :-
    (Punt1 > Punt2 -> 
        Orden = (<)
    ; Punt1 < Punt2 ->
        Orden = (>)
    ; 
        Orden = (=)
    ).

% Extract menu from scored menu
extraer_menu(menu_puntuado(Menu, _), Menu).

% Simple menu scoring for safe prioritization
simple_menu_score(Menu, menu_score(Menu, Score)) :-
    aplicar_reglas_aprendidas(Menu, Score).

% Extract menu from simple score structure  
extract_menu_from_score(menu_score(Menu, _), Menu).

% Induce preference rules from current knowledge base
% inducir_reglas_preferencia(-ReglasAprendidas)
inducir_reglas_preferencia(ReglasAprendidas) :-
    findall(regla(Componente, Valor, Confianza),
            (regla_preferencia(Componente, Valor, _),
             calcular_confianza_regla(Componente, Valor, Confianza),
             Confianza >= 0.7),
            ReglasTemp),
    sort(ReglasTemp, ReglasAprendidas).

% Get prioritized menus using learned rules
get_menus_priorizados(VegetarianFilter, MeatTypeFilter, ConPostre, MinCal, MaxCal, MenusPriorizados) :-
    generate_all_menus(VegetarianFilter, MeatTypeFilter, ConPostre, MinCal, MaxCal, TodosMenus),
    priorizar_menus(TodosMenus, MenusPriorizados).

% Get prioritized menus with ingredient filtering
get_menus_priorizados_con_ingredientes(VegetarianFilter, MeatTypeFilter, ConPostre, MinCal, MaxCal, 
                                      IncludeList, ExcludeList, MenusPriorizados) :-
    findall(Menu, 
            generate_menu_with_ingredients(VegetarianFilter, MeatTypeFilter, ConPostre, MinCal, MaxCal, 
                                         IncludeList, ExcludeList, Menu), 
            TodosMenus),
    priorizar_menus(TodosMenus, MenusPriorizados).

% ============================================================================
% INTEGRATED MENU RETRIEVAL WITH LEARNING AND DIVERSITY
% ============================================================================

% Generate and prioritize menus with learning, returning diverse results
% get_menus_con_aprendizaje(+VegetarianFilter, +MeatTypeFilter, +ConPostre, 
%                           +MinCal, +MaxCal, +IncludeList, +ExcludeList, 
%                           +MaxMenus, -MenusFinales)
get_menus_con_aprendizaje(VegetarianFilter, MeatTypeFilter, ConPostre, 
                          MinCal, MaxCal, IncludeList, ExcludeList, 
                          MaxMenus, MenusFinales) :-
    % NUEVA IMPLEMENTACIÓN: Usar algoritmo de diversidad mejorado
    
    % Step 1: Generar muchos más candidatos para mayor diversidad
    findnsols(150, Menu, 
            generate_menu_with_ingredients(VegetarianFilter, MeatTypeFilter, ConPostre, 
                                          MinCal, MaxCal, IncludeList, ExcludeList, Menu), 
            TodosCandidatos),
    
    % Step 2: Aplicar algoritmo de diversidad mejorado
    (TodosCandidatos \= [] ->
        select_diverse_menus_improved(TodosCandidatos, MaxMenus, MenusDiversos),
        
        % Step 3: Aplicar aprendizaje solo a los menús diversos seleccionados
        maplist(aplicar_puntuacion_aprendizaje, MenusDiversos, MenusConPuntuacion),
        sort(MenusConPuntuacion, MenusOrdenadosPorAprendizaje),
        reverse(MenusOrdenadosPorAprendizaje, MenusFinalesConPunt),  % Mayor puntuación primero
        
        % Step 4: Extraer solo los menús (sin puntuaciones) 
        maplist(extraer_menu_de_puntuacion, MenusFinalesConPunt, MenusFinales)
    ;
        MenusFinales = []
    ).

% Aplicar puntuación de aprendizaje a un menú individual
aplicar_puntuacion_aprendizaje(Menu, puntuacion(Puntuacion, Menu)) :-
    aplicar_reglas_aprendidas(Menu, Puntuacion).

% Extraer menú de estructura puntuacion(Punt, Menu)
extraer_menu_de_puntuacion(puntuacion(_, Menu), Menu).

% Helper: Extract menu details from learned prioritized menus
% get_menu_details_con_aprendizaje(+VegetarianFilter, +MeatTypeFilter, +ConPostre, 
%                                  +MinCal, +MaxCal, +IncludeList, +ExcludeList,
%                                  -EntradaNom, -EntradaCal, -EntradaVeg,
%                                  -CarbNom, -CarbCal, -CarbVeg,
%                                  -CarneNom, -CarneCal, -CarneTipo, -CarneVeg,
%                                  -VegNom, -VegCal, -VegVeg,
%                                  -PostreNom, -PostreCal, -PostreVeg,
%                                  -TotalCal)
% Fixed version with safe learning integration
get_menu_details_con_aprendizaje(VegetarianFilter, MeatTypeFilter, ConPostre, 
                                MinCal, MaxCal, IncludeList, ExcludeList,
                                EntradaNom, EntradaCal, EntradaVeg,
                                CarbNom, CarbCal, CarbVeg,
                                CarneNom, CarneCal, CarneTipo, CarneVeg,
                                VegNom, VegCal, VegVeg,
                                PostreNom, PostreCal, PostreVeg,
                                TotalCal) :-
    % Use safe learning integration with proper limits
    get_menus_con_aprendizaje(VegetarianFilter, MeatTypeFilter, ConPostre,
                              MinCal, MaxCal, IncludeList, ExcludeList,
                              10, MenusConAprendizaje),
    member(Menu, MenusConAprendizaje),
    extract_menu_components(Menu,
                           EntradaNom, EntradaCal, EntradaVeg,
                           CarbNom, CarbCal, CarbVeg,
                           CarneNom, CarneCal, CarneTipo, CarneVeg,
                           VegNom, VegCal, VegVeg,
                           PostreNom, PostreCal, PostreVeg,
                           TotalCal).

% Nueva versión que acepta cantidad personalizable
get_menu_details_con_cantidad(VegetarianFilter, MeatTypeFilter, ConPostre, 
                             MinCal, MaxCal, IncludeList, ExcludeList, CantidadMenus,
                             EntradaNom, EntradaCal, EntradaVeg,
                             CarbNom, CarbCal, CarbVeg,
                             CarneNom, CarneCal, CarneTipo, CarneVeg,
                             VegNom, VegCal, VegVeg,
                             PostreNom, PostreCal, PostreVeg,
                             TotalCal) :-
    % Usar la nueva estrategia eficiente con cantidad personalizable
    get_menus_con_aprendizaje(VegetarianFilter, MeatTypeFilter, ConPostre,
                              MinCal, MaxCal, IncludeList, ExcludeList,
                              CantidadMenus, MenusConAprendizaje),
    member(Menu, MenusConAprendizaje),
    extract_menu_components(Menu,
                           EntradaNom, EntradaCal, EntradaVeg,
                           CarbNom, CarbCal, CarbVeg,
                           CarneNom, CarneCal, CarneTipo, CarneVeg,
                           VegNom, VegCal, VegVeg,
                           PostreNom, PostreCal, PostreVeg,
                           TotalCal).
