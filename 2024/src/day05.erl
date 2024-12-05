-module(day05).
-export([solve_1/1, solve_2/1]).

% Abi dalys buvo nesunkios. Tik daug visokių pagalbinių funkcijų prisirašyti
% reikėjo. Pradžiai galvojau, ar bet kurie du skaičiai bus palyginti prie
% apribojimų. Nusprendžiau daryti tokią prielaidą ir neapsirikau. Taip pat
% galvojau daryti quick sort rūšiavimą antroje dalyje, bet supratau, kad
% paprasčiau bus padaryti selection sort. Pasirodo, to ir užteko, nes sąrašai
% nebuvo dideli. O jau pabaigęs ir Rimo paprotintas perdariau į Erlango sort
% su paduota lyginimo funkcija.

% (aoc_2024@JuliusErisataT14.erisata.lt)1> day05:solve_1("priv/day05-PVZ.txt").
% 143
% (aoc_2024@JuliusErisataT14.erisata.lt)2> day05:solve_1("priv/day05.txt").
% 5955
% (aoc_2024@JuliusErisataT14.erisata.lt)3> day05:solve_2("priv/day05-PVZ.txt").
% 123
% (aoc_2024@JuliusErisataT14.erisata.lt)4> day05:solve_2("priv/day05.txt").
% 4030


read_inputs(FileName) ->
    ReadConstraintsFun = fun(Line) ->
        LineNoNL = utils:drop_trailing_new_line(Line),
        [_, _] = utils:get_integer_list(LineNoNL, "|")
    end,
    ReadUpdatesFun = fun(Line) ->
        LineNoNL = utils:drop_trailing_new_line(Line),
        utils:get_integer_list(LineNoNL, ",")
    end,
    [ConstraintsList, Updates] = utils:read_lines_to_elems(FileName, [ReadConstraintsFun, ReadUpdatesFun], "\n"),
    Constraints = lists:foldl(fun([Nr1, Nr2], AccConstraints) ->
        case maps:get(Nr1, AccConstraints, undefined) of
            undefined -> AccConstraints#{Nr1 => [Nr2]};
            Numbers   -> AccConstraints#{Nr1 => [Nr2|Numbers]}
        end
    end, #{}, ConstraintsList),
    {Updates, Constraints}.


is_update_right_order([], _) ->
    true;

is_update_right_order([Nr1|OtherUpdate], Constraints) ->
    NrOrder = lists:all(fun(Nr2) ->
        case maps:get(Nr2, Constraints, undefined) of
            undefined  -> true;
            Constraint -> not(lists:member(Nr1, Constraint))
        end
    end, OtherUpdate),
    case NrOrder of
        true  -> is_update_right_order(OtherUpdate, Constraints);
        false -> false
    end.


solve_1(FileName) ->
    {Updates, Constraints} = read_inputs(FileName),
    UpdatesR = lists:filter(fun(Update) -> is_update_right_order(Update, Constraints) end, Updates),
    lists:sum(lists:map(fun utils:middle_single/1/1, UpdatesR)).


solve_2(FileName) ->
    {Updates, Constraints} = read_inputs(FileName),
    UpdatesW = lists:filter(fun(Update) -> not(is_update_right_order(Update, Constraints)) end, Updates),
    CompareNrsFun = fun(Nr1, Nr2) ->
        ConstraintNr1 = maps:get(Nr1, Constraints, []),
        ConstraintNr2 = maps:get(Nr2, Constraints, []),
        case {lists:member(Nr2, ConstraintNr1), lists:member(Nr1, ConstraintNr2)} of
            {true, false} -> true;
            {false, true} -> false
        end
    end,
    CorrectMiddleFun = fun(Update) ->
        UpdateC = lists:sort(CompareNrsFun, Update),
        utils:middle_single(UpdateC)
    end,
    lists:sum(lists:map(CorrectMiddleFun, UpdatesW)).
