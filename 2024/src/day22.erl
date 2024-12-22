-module(day22).
-export([solve_1/1, solve_2/1]).

% Net keista, koks šitas buvo paprastas. Viską padariau tiesiai be jokių
% ypatingų klaidžiojimų. Tik aprašymai baisiai ilgi. Bet ką jau padarysi.

% (aoc_2024@JuliusErisataT14.erisata.lt)1> day22:solve_1("priv/day22-PVZ1.txt").
% 37327623
% (aoc_2024@JuliusErisataT14.erisata.lt)2> day22:solve_1("priv/day22.txt").
% 19458130434
% (aoc_2024@JuliusErisataT14.erisata.lt)3> day22:solve_2("priv/day22-PVZ2.txt").
% 23
% (aoc_2024@JuliusErisataT14.erisata.lt)4> day22:solve_2("priv/day22.txt").
% 2130
% (aoc_2024@JuliusErisataT14.erisata.lt)5> timer:tc(fun() -> day22:solve_1("priv/day22.txt") end).
% {151777,19458130434}
% (aoc_2024@JuliusErisataT14.erisata.lt)6> timer:tc(fun() -> day22:solve_2("priv/day22.txt") end).
% {7143020,2130}


read_inputs(FileName) ->
    utils:read_lines_no_new_line_to_elems(FileName, fun(Line) ->
        {Int, ""} = utils:get_integer(Line),
        Int
    end).


mix(Secret, Op) -> Secret bxor Op.

prune(Secret) -> Secret band 16777215.


next_secret(Secret) ->
    Secret1 = prune(mix(Secret, Secret bsl 6)),
    Secret2 = prune(mix(Secret1, Secret1 bsr 5)),
    Secret3 = prune(mix(Secret2, Secret2 bsl 11)),
    Secret3.


nth_secret(0, Secret) -> Secret;
nth_secret(N, Secret) ->
    NewSecret = next_secret(Secret),
    nth_secret(N-1, NewSecret).


find_best_changes_buyer([_P1,_P2,_P3,_P4], _AccSeen, AccCache) ->
    AccCache;

find_best_changes_buyer([P1,P2,P3,P4,P5|Prices], AccSeen, AccCache) ->
    Diffs = {P2 - P1, P3 - P2, P4 - P3, P5 - P4},
    {NewAccSeen, NewAccCache} = case maps:get(Diffs, AccSeen, undefined) of
        true ->
            {AccSeen, AccCache};
        undefined ->
            AccBananas = maps:get(Diffs, AccCache, 0),
            NewAccBananas = AccBananas + P5,
            {AccSeen#{Diffs => true}, AccCache#{Diffs => NewAccBananas}}
    end,
    find_best_changes_buyer([P2,P3,P4,P5|Prices], NewAccSeen, NewAccCache).



find_best_changes_buyers(PricesList) ->
    Cache = lists:foldl(fun(Prices, AccCache) ->
        find_best_changes_buyer(Prices, #{}, AccCache)
    end, #{}, PricesList),
    maps:fold(fun(_, Price, Acc) ->
        lists:max([Price, Acc])
    end, 0, Cache).



solve_1(FileName) ->
    %lists:foldl(fun(_, S) -> NS = next_secret(S), utils:print("~p", [NS]), NS end, 123, lists:seq(1, 10)). % TEST
    Secrets = read_inputs(FileName),
    utils:list_map_sum(fun(Secret) ->
        nth_secret(2000, Secret)
    end, Secrets).


solve_2(FileName) ->
    Secrets = read_inputs(FileName),
    PricesList = lists:map(fun(Secret) ->
        {Prices, _} = lists:mapfoldl(fun(_, S) ->
            NS = next_secret(S),
            {NS rem 10, NS}
        end, Secret, lists:seq(1, 2000)),
        Prices
    end, Secrets),
    find_best_changes_buyers(PricesList).
