-module(day05).
-export([solve_1/1, solve_2/1]).

% Abi dalys buvo nesunkios. Tik daug visokių pagalbinių funkcijų prisirašyti
% reikėjo. Pradžiai galvojau, ar bet kurie du skaičiai bus palyginti prie
% apribojimų. Nusprendžiau daryti tokią prielaidą ir neapsirikau. Taip pat
% galvojau daryti quick sort rūšiavimą antroje dalyje, bet supratau, kad
% paprasčiau bus padaryti selection sort. Pasirodo, to ir užteko, nes sąrašai
% nebuvo dideli.

% (aoc_2024@JuliusErisataT14.erisata.lt)1> day05:solve_1("priv/day05-PVZ.txt").
% 143
% (aoc_2024@JuliusErisataT14.erisata.lt)2> day05:solve_1("priv/day05.txt").
% 5955
% (aoc_2024@JuliusErisataT14.erisata.lt)3> day05:solve_2("priv/day05-PVZ.txt").
% 123
% (aoc_2024@JuliusErisataT14.erisata.lt)4> day05:solve_2("priv/day05.txt").
% 4030


get_integer_list(Line) ->
    NewLine = string:left(Line, erlang:length(Line)-1),
    IntegerStrs = string:split(NewLine, ",", all),
    lists:map(fun(IntegerStr) -> erlang:list_to_integer(IntegerStr) end, IntegerStrs).


read_inputs(FileName) ->
    Lines = lists:reverse(utils:read_lines(FileName)),
    ReadConstraintsFun = fun
        ReadConstraintsFun(["\n"|OtherLines], AccConstraints) ->
            {AccConstraints, OtherLines};
        ReadConstraintsFun([Line|OtherLines], AccConstraints) ->
            {Nr1, [$| |NewLine]} = utils:get_integer(Line),
            {Nr2, "\n"         } = utils:get_integer(NewLine),
            NewAccConstraints = case maps:get(Nr1, AccConstraints, undefined) of
                undefined -> AccConstraints#{Nr1 => [Nr2]};
                Numbers   -> AccConstraints#{Nr1 => [Nr2|Numbers]}
            end,
            ReadConstraintsFun(OtherLines, NewAccConstraints)
    end,
    {Constraints, NewLines} = ReadConstraintsFun(Lines, #{}),
    Updates = lists:map(fun get_integer_list/1, NewLines),
    {Updates, Constraints}.


update_right_order([], _) ->
    true;

update_right_order([Nr1|OtherUpdate], Constraints) ->
    NrOrder = lists:all(fun(Nr2) ->
        case maps:get(Nr2, Constraints, undefined) of
            undefined  -> true;
            Constraint -> not(lists:member(Nr1, Constraint))
        end
    end, OtherUpdate),
    case NrOrder of
        true  -> update_right_order(OtherUpdate, Constraints);
        false -> false
    end.


first_update(CurrFirst, [], _, AccUpdates) ->
    {CurrFirst, AccUpdates};

first_update(CurrFirst, [MaybeFirst|Updates], Constraints, AccUpdates) ->
    ConstraintCurr  = maps:get(CurrFirst, Constraints, undefined),
    ConstraintMaybe = maps:get(MaybeFirst, Constraints, undefined),
    Which = case {ConstraintCurr, ConstraintMaybe} of
        {undefined, _} ->
            maybee;
        {_, undefined} ->
            curr;
        _ ->
            case {lists:member(MaybeFirst, ConstraintCurr), lists:member(CurrFirst, ConstraintMaybe)} of
                {true, false} -> curr;
                {false, true} -> maybee
            end
    end,
    case Which of
        curr   -> first_update(CurrFirst,  Updates, Constraints, [MaybeFirst|AccUpdates]);
        maybee -> first_update(MaybeFirst, Updates, Constraints, [CurrFirst|AccUpdates])
    end.


update_correct_order([], _) ->
    [];

update_correct_order([Nr1|Updates], Constraints) ->
    {First, RemainingUpdates} = first_update(Nr1, Updates, Constraints, []),
    [First | update_correct_order(RemainingUpdates, Constraints)].



middle(List) ->
    Length = erlang:length(List),
    Half = Length div 2,
    case Length rem 2  of
        0 -> [lists:nth(Half, List), lists:nth(Half+1, List) ];
        1 -> [lists:nth(Half + 1, List)]
    end.


single_middle(List) ->
    [Middle] = middle(List),
    Middle.


solve_1(FileName) ->
    {Updates, Constraints} = read_inputs(FileName),
    UpdatesR = lists:filter(fun(Update) -> update_right_order(Update, Constraints) end, Updates),
    lists:sum(lists:map(fun single_middle/1, UpdatesR)).


solve_2(FileName) ->
    {Updates, Constraints} = read_inputs(FileName),
    UpdatesW = lists:filter(fun(Update) -> not(update_right_order(Update, Constraints)) end, Updates),
    CorrectMiddleFun = fun(Update) ->
        UpdateC = update_correct_order(Update, Constraints),
        single_middle(UpdateC)
    end,
    lists:sum(lists:map(CorrectMiddleFun, UpdatesW)).
