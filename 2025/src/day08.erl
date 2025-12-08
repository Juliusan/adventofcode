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


take(Elem, List) ->
    case take(Elem, List, []) of
        undefined -> {undefined, List};
        Result    -> Result
    end.

take(_Fun, [], _AccList) -> undefined;
take( Fun, [ Elem|RemList], AccList) ->
    case Fun(Elem) of
        true  -> {Elem, lists:reverse(AccList) ++ RemList};
        false -> take(Fun, RemList, [Elem|AccList])
    end.


solve_1(FileName, BoxPairsToConnect) ->
    Boxes = ja_erl_utils_file:read_lines_no_new_line_to_elems(FileName, fun(Line) ->
        ja_erl_utils_string:get_integer_list(Line, ",")
    end),
    %ja_erl_utils_terminal:print("~p", [Boxes]),
    BoxDistances = ja_erl_utils_list:foldl_pairs(fun(Box1, Box2, AccBoxDistances) ->
        Distance = distance(Box1, Box2),
        [{Distance, Box1, Box2}|AccBoxDistances]
    end, [], Boxes),
    BoxDistancesSorted = lists:sublist(lists:sort(BoxDistances), BoxPairsToConnect),
    %ja_erl_utils_terminal:print("~p", [BoxDistancesSorted]),
    BoxGroups = lists:foldl(fun({_, Box1, Box2}, AccGroups) ->
        BoxInGroupFunFun = fun(Box) ->
            fun(Group) -> lists:member(Box, Group) end
        end,
        case take(BoxInGroupFunFun(Box1), AccGroups) of
            {undefined, AccGroups} ->
                case take(BoxInGroupFunFun(Box2), AccGroups) of
                    {undefined, RemAccGroups2} -> [[Box1, Box2]|RemAccGroups2];
                    {Group,     RemAccGroups2} -> [[Box1|Group]|RemAccGroups2]
                end;
            {Group1, RemAccGroups1} ->
                BoxInGroup2Fun = BoxInGroupFunFun(Box2),
                case BoxInGroup2Fun(Group1) of
                    true ->
                        [Group1|RemAccGroups1];
                    false ->
                        case take(BoxInGroupFunFun(Box2), RemAccGroups1) of
                            {undefined, RemAccGroups2} -> [[Box2|Group1]|RemAccGroups2];
                            {Group2,    RemAccGroups2} -> [Group1++Group2|RemAccGroups2]
                        end
                end
        end
    end, [], BoxDistancesSorted),
    [L1, L2, L3|_] = lists:reverse(lists:sort(lists:map(fun(Group) -> erlang:length(Group) end, BoxGroups))),
    L1*L2*L3.


solve_2(FileName) ->
    Boxes = ja_erl_utils_file:read_lines_no_new_line_to_elems(FileName, fun(Line) ->
        ja_erl_utils_string:get_integer_list(Line, ",")
    end),
    %ja_erl_utils_terminal:print("~p", [Boxes]),
    BoxesCount = erlang:length(Boxes),
    BoxDistances = ja_erl_utils_list:foldl_pairs(fun(Box1, Box2, AccBoxDistances) ->
        Distance = distance(Box1, Box2),
        [{Distance, Box1, Box2}|AccBoxDistances]
    end, [], Boxes),
    BoxDistancesSorted = lists:sort(BoxDistances),
    %ja_erl_utils_terminal:print("~p", [BoxDistancesSorted]),
    GroupBoxesFun = fun GroupBoxesFun([{_, Box1, Box2}|RemPairs], AccGroups) ->
        BoxInGroupFunFun = fun(Box) ->
            fun(Group) -> lists:member(Box, Group) end
        end,
        NewAccGroups = case take(BoxInGroupFunFun(Box1), AccGroups) of
            {undefined, AccGroups} ->
                case take(BoxInGroupFunFun(Box2), AccGroups) of
                    {undefined,  RemAccGroups2} -> [[Box1, Box2]|RemAccGroups2];
                    {Group2,     RemAccGroups2} -> [[Box1|Group2]|RemAccGroups2]
                end;
            {Group1, RemAccGroups1} ->
                BoxInGroup2Fun = BoxInGroupFunFun(Box2),
                case BoxInGroup2Fun(Group1) of
                    true ->
                        [Group1|RemAccGroups1];
                    false ->
                        case take(BoxInGroupFunFun(Box2), RemAccGroups1) of
                            {undefined, RemAccGroups2} -> [[Box2|Group1]|RemAccGroups2];
                            {Group2,    RemAccGroups2} -> [Group1++Group2|RemAccGroups2]
                        end
                end
        end,
        %ja_erl_utils_terminal:print("~p", [NewAccGroups]),
        case NewAccGroups of
            [Group] when erlang:length(Group) =:= BoxesCount -> {Box1, Box2};
            _                                                       -> GroupBoxesFun(RemPairs, NewAccGroups)
        end
    end,
    {[X1,_,_],[X2,_,_]} = GroupBoxesFun(BoxDistancesSorted, []),
    X1*X2.
