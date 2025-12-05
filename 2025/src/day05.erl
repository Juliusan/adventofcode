-module(day05).
-export([solve_1/1, solve_2/1]).


% Aš jau pirmojoje dalyje galvojau jungti intervalus. Bet paskui nusprendžiau,
% kad nereikia. Aš turiu išankstinių optimizacijų bėdą ir taip prisidarau sau
% darbo. Tai pirmąją dalį padariau paprastai. Bet antrojoje vis dėl to to
% prireikė. Tai nesunkiai padariau. Tik vieną klaidą įvėliau, kuri kainavo man
% 5 minutes laiko. Žodžiu, lengva užduotis buvo.

% (aoc_2025@JuliusErisataT14.erisata.lt)1> day05:solve_1("priv/day05-PVZ.txt").
% 3
% (aoc_2025@JuliusErisataT14.erisata.lt)2> day05:solve_1("priv/day05.txt").
% 617
% (aoc_2025@JuliusErisataT14.erisata.lt)3> day05:solve_2("priv/day05-PVZ.txt").
% 14
% (aoc_2025@JuliusErisataT14.erisata.lt)4> day05:solve_2("priv/day05.txt").
% 338258295736104
% (aoc_2025@JuliusErisataT14.erisata.lt)5> timer:tc(fun() -> day05:solve_1("priv/day05.txt") end).
% {13167,617}
% (aoc_2025@JuliusErisataT14.erisata.lt)6> timer:tc(fun() -> day05:solve_2("priv/day05.txt") end).
% {11122,338258295736104}


read(FileName) ->
    [IdStrs, RangeStrs] = ja_erl_utils_file:read_line_groups_no_new_line(FileName, fun(Groups) -> Groups end, "\n"),
    Ranges = lists:sort(lists:map(fun(RangeStr) ->
        [From, To] = string:split(RangeStr, "-"),
        {erlang:list_to_integer(From), erlang:list_to_integer(To)}
    end, RangeStrs)),
    Ids = lists:map(fun erlang:list_to_integer/1, IdStrs),
    %ja_erl_utils_terminal:print("~p~n~p", [Ranges, Ids]),
    JoinRangesFun = fun
        JoinRangesFun([], AccJoinedRanges) ->
            AccJoinedRanges;
        JoinRangesFun([{From, To}|RemRanges], AccJoinedRanges) ->
            JoinRangeFun = fun
                JoinRangeFun([{From2, To2}|RemRanges2], AccTo) when From =< From2, From2 =< AccTo ->
                    NewAccTo = erlang:max(To2, AccTo),
                    JoinRangeFun(RemRanges2, NewAccTo);
                JoinRangeFun(RemRanges2, AccTo) ->
                    {AccTo, RemRanges2}
            end,
            {FinalTo, FinalRemRanges} = JoinRangeFun(RemRanges, To),
            JoinRangesFun(FinalRemRanges, [{From, FinalTo}|AccJoinedRanges])
    end,
    JoinedRanges = JoinRangesFun(Ranges, []),
    %ja_erl_utils_terminal:print("~p", [JoinedRanges]),
    {JoinedRanges, Ids}.


solve_1(FileName) ->
    {Ranges, Ids} = read(FileName),
    ja_erl_utils_list:filter_count(fun(Id) ->
        lists:any(fun({From, To}) -> From =< Id andalso Id =< To end, Ranges)
    end, Ids).


solve_2(FileName) ->
    {Ranges, _Ids} = read(FileName),
    ja_erl_utils_list:map_sum(fun({From, To}) -> To - From + 1 end, Ranges).
