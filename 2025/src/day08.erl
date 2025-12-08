-module(day08).
-export([solve_1/2, solve_2/1]).


% Keista, bet ir vėl paprasčiausias algoritmas suveikė: tiesiog suskaičiuoji
% visų dėžių porų atstumus, juos susirūšiuoji, imi po vieną porą ir tikrini, ar
% jau baigta. Abi dalys sukosi pastebimai ne akimirką, bet vis tiek tilpo į 1
% sekundę. Užtruko, kol surašiau pirmąją dalį, bet pakopijuoti antrąją truko
% pakankamai trumpai.

% (aoc_2025@JuliusErisataT14.erisata.lt)1> day08:solve_1("priv/day08-PVZ.txt", 10).
% 40
% (aoc_2025@JuliusErisataT14.erisata.lt)2> day08:solve_1("priv/day08.txt", 1000).
% 52668
% (aoc_2025@JuliusErisataT14.erisata.lt)3> day08:solve_2("priv/day08-PVZ.txt").
% 25272
% (aoc_2025@JuliusErisataT14.erisata.lt)4> day08:solve_2("priv/day08.txt").
% 1474050600
% (aoc_2025@JuliusErisataT14.erisata.lt)5> timer:tc(fun() -> day08:solve_1("priv/day08.txt", 1000) end).
% {843867,52668}
% (aoc_2025@JuliusErisataT14.erisata.lt)6> timer:tc(fun() -> day08:solve_2("priv/day08.txt") end).
% {698646,1474050600}


distance(Box1, Box2) ->
    [X1, Y1, Z1] = Box1,
    [X2, Y2, Z2] = Box2,
    math:pow(X2-X1, 2) + math:pow(Y2-Y1, 2) + math:pow(Z2-Z1, 2).


read(FileName) ->
    Boxes = ja_erl_utils_file:read_lines_no_new_line_to_elems(FileName, fun(Line) ->
        ja_erl_utils_string:get_integer_list(Line, ",")
    end),
    %ja_erl_utils_terminal:print("~p", [Boxes]),
    BoxDistances = ja_erl_utils_list:foldl_pairs(fun(Box1, Box2, AccBoxDistances) ->
        Distance = distance(Box1, Box2),
        [{Distance, Box1, Box2}|AccBoxDistances]
    end, [], Boxes),
    BoxDistancesSorted = lists:sort(BoxDistances),
    %ja_erl_utils_terminal:print("~p", [BoxDistancesSorted]),
    {Boxes, BoxDistancesSorted}.


group_boxes(Pairs, EndFun) ->
    group_boxes(Pairs, [], EndFun).

group_boxes([{_, Box1, Box2}|RemPairs], AccGroups, EndFun) ->
    NewAccGroups = case utils:ja_erl_utils_list_take(fun(Group) -> lists:member(Box1, Group) end, AccGroups) of
        {undefined, AccGroups} ->
            case utils:ja_erl_utils_list_take(fun(Group) -> lists:member(Box2, Group) end, AccGroups) of
                {undefined,  RemAccGroups2} -> [[Box1, Box2]|RemAccGroups2];
                {Group2,     RemAccGroups2} -> [[Box1|Group2]|RemAccGroups2]
            end;
        {Group1, RemAccGroups1} ->
            case lists:member(Box2, Group1) of
                true ->
                    [Group1|RemAccGroups1];
                false ->
                    case utils:ja_erl_utils_list_take(fun(Group) -> lists:member(Box2, Group) end, RemAccGroups1) of
                        {undefined, RemAccGroups2} -> [[Box2|Group1]|RemAccGroups2];
                        {Group2,    RemAccGroups2} -> [Group1++Group2|RemAccGroups2]
                    end
            end
    end,
    %ja_erl_utils_terminal:print("~p", [NewAccGroups]),
    case EndFun(NewAccGroups, RemPairs) of
        true  -> {{Box1, Box2}, NewAccGroups};
        _     -> group_boxes(RemPairs, NewAccGroups, EndFun)
    end.


solve_1(FileName, BoxPairsToConnect) ->
    {_, BoxDistancesAll} = read(FileName),
    BoxDistances = lists:sublist(BoxDistancesAll, BoxPairsToConnect),
    {_, BoxGroups} = group_boxes(BoxDistances, fun
        (_, []   ) -> true;
        (_, [_|_]) -> false
    end),
    [L1, L2, L3|_] = lists:reverse(lists:sort(lists:map(fun(Group) -> erlang:length(Group) end, BoxGroups))),
    L1*L2*L3.


solve_2(FileName) ->
    {Boxes, BoxDistances} = read(FileName),
    BoxesCount = erlang:length(Boxes),
    {{[X1,_,_],[X2,_,_]}, _BoxGroups} = group_boxes(BoxDistances, fun
        ([Group], _) -> erlang:length(Group) =:= BoxesCount;
        ([_|_],   _) -> false
    end),
    X1*X2.
