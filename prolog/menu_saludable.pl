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
entrada('Ceviche', 160, false).

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
filter_meat_type(MeatType, _, _, Type, _) :-
    MeatType = Type.

% Filter ingredients to include specific ones
% filter_ingredientes_incluir(+IngredientName, +NombreItem, +ListaIncluir, +Result)
filter_ingredientes_incluir(_, _, [], true).
filter_ingredientes_incluir(IngredientName, NombreItem, ListaIncluir, Result) :-
    ListaIncluir \= [],
    (member(IngredientName, ListaIncluir) ; member(NombreItem, ListaIncluir)),
    Result = true.
filter_ingredientes_incluir(IngredientName, NombreItem, ListaIncluir, Result) :-
    ListaIncluir \= [],
    \+ member(IngredientName, ListaIncluir),
    \+ member(NombreItem, ListaIncluir),
    Result = false.

% Filter ingredients to exclude specific ones
% filter_ingredientes_excluir(+IngredientName, +NombreItem, +ListaExcluir, +Result)
filter_ingredientes_excluir(_, _, [], true).
filter_ingredientes_excluir(IngredientName, NombreItem, ListaExcluir, Result) :-
    ListaExcluir \= [],
    (member(IngredientName, ListaExcluir) ; member(NombreItem, ListaExcluir)),
    Result = false.
filter_ingredientes_excluir(IngredientName, NombreItem, ListaExcluir, Result) :-
    ListaExcluir \= [],
    \+ member(IngredientName, ListaExcluir),
    \+ member(NombreItem, ListaExcluir),
    Result = true.

% ============================================================================
% INGREDIENT VALIDATION
% ============================================================================

% Check if an ingredient exists in any category
ingredient_exists(IngredientName) :-
    entrada(IngredientName, _, _) ;
    carbohidrato(IngredientName, _, _) ;
    carne(IngredientName, _, _, _) ;
    vegetal(IngredientName, _, _) ;
    postre(IngredientName, _, _).

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

% Generate a valid menu combination with ingredient filtering
% generate_menu_with_ingredients(VegetarianFilter, MeatTypeFilter, ConPostre, MinCal, MaxCal, IncludeList, ExcludeList, Menu)
generate_menu_with_ingredients(VegetarianFilter, MeatTypeFilter, ConPostre, MinCal, MaxCal, IncludeList, ExcludeList,
              menu(Entrada, Carbohidrato, Carne, Vegetal, Postre, TotalCalorias)) :-
    % Validate ingredient lists first
    validate_ingredients_list(IncludeList),
    validate_ingredients_list(ExcludeList),
    
    % Get filtered items
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
% MENU DIVERSITY
% ============================================================================

% Count differences between two menus
count_differences(menu(E1, C1, M1, V1, _, _), menu(E2, C2, M2, V2, _, _), Count) :-
    (E1 \= E2 -> Diff1 = 1 ; Diff1 = 0),
    (C1 \= C2 -> Diff2 = 1 ; Diff2 = 0),
    (M1 \= M2 -> Diff3 = 1 ; Diff3 = 0),
    (V1 \= V2 -> Diff4 = 1 ; Diff4 = 0),
    Count is Diff1 + Diff2 + Diff3 + Diff4.

% Check if a menu is sufficiently different from a list of menus
is_sufficiently_different(_, [], true).
is_sufficiently_different(Menu, [H|T], Result) :-
    count_differences(Menu, H, Diff),
    (Diff >= 2 ->
        is_sufficiently_different(Menu, T, Result)
    ;
        Result = false
    ).

% Select diverse menus from a list
select_diverse_menus([], _, MaxMenus, MaxMenus, []).
select_diverse_menus(_, Selected, 0, _, Selected).
select_diverse_menus([H|T], Accumulated, Remaining, MaxMenus, Result) :-
    Remaining > 0,
    is_sufficiently_different(H, Accumulated, true),
    !,
    NewAccumulated = [H|Accumulated],
    NewRemaining is Remaining - 1,
    select_diverse_menus(T, NewAccumulated, NewRemaining, MaxMenus, Result).
select_diverse_menus([_|T], Accumulated, Remaining, MaxMenus, Result) :-
    Remaining > 0,
    select_diverse_menus(T, Accumulated, Remaining, MaxMenus, Result).

% Main predicate to get diverse menus
get_diverse_menus(VegetarianFilter, MeatTypeFilter, ConPostre, MinCal, MaxCal, MaxMenus, DiverseMenus) :-
    generate_all_menus(VegetarianFilter, MeatTypeFilter, ConPostre, MinCal, MaxCal, AllMenus),
    length(AllMenus, TotalMenus),
    (TotalMenus =< MaxMenus ->
        DiverseMenus = AllMenus
    ;
        select_diverse_menus(AllMenus, [], MaxMenus, MaxMenus, ReversedMenus),
        reverse(ReversedMenus, DiverseMenus)
    ).

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
