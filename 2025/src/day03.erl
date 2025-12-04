-module(day03).
-export([solve_1/1, solve_2/1]).

% Labai neturiu laiko spręsti. Prieš važiuodamas namo iš darbo pažiūrėjau
% užduotį, važiuodamas sugalvojau sprendimą ir naktį jį užrašiau. Algoritmas
% geras. Užrašyti greitai neišėjo ir dar po pirmos dalies buvau palikęs vieną
% klaidą kode. Jei ne ji, antrąją dalį būčiau per minutę išsprendęs.

% (aoc_2025@JuliusErisataT14.erisata.lt)1> day03:solve_1("priv/day03-PVZ.txt").
% 357
% (aoc_2025@JuliusErisataT14.erisata.lt)2> day03:solve_1("priv/day03.txt").
% 17113
% (aoc_2025@JuliusErisataT14.erisata.lt)3> day03:solve_2("priv/day03-PVZ.txt").
% 3121910778619
% (aoc_2025@JuliusErisataT14.erisata.lt)4> day03:solve_2("priv/day03.txt").
% 169709990062889
% (aoc_2025@JuliusErisataT14.erisata.lt)5> timer:tc(fun() -> day03:solve_1("priv/day03.txt") end).
% {7239,17113}
% (aoc_2025@JuliusErisataT14.erisata.lt)6> timer:tc(fun() -> day03:solve_2("priv/day03.txt") end).
% {5484,169709990062889}


on_to_int(On, BatteriesOnInBank) ->
    lists:foldl(fun(Index, AccInt) ->
        {_Index, Battery} = maps:get(Index, On),
        AccInt * 10 + Battery
    end, 0, lists:seq(0, BatteriesOnInBank-1)).


solve(FileName, BatteriesOnInBank) ->
    Banks = ja_erl_utils_file:read_lines_no_new_line_to_elems(FileName, fun(Line) ->
        lists:map(fun(Char) -> Char - $0 end, Line)
    end),
    %ja_erl_utils_terminal:print("~p", [Banks]),
    GenerateOnFun = fun(InitIndex, InitOnIndex, InitOn, InitRemBank) ->
        {_, _, On} = lists:foldl(fun(OnIndex, {Index, RemBank, AccOn}) ->
            [Battery|NewRemBank] = RemBank,
            {Index+1, NewRemBank, AccOn#{OnIndex => {Index, Battery}}}
        end, {InitIndex, InitRemBank, InitOn}, lists:seq(InitOnIndex, BatteriesOnInBank-1)),
        On
    end,
    ja_erl_utils_list:map_sum(fun(Bank) ->
        %ja_erl_utils_terminal:print("~p", [GenerateOnFun(0, #{}, Bank)]),
        BankSize = erlang:length(Bank),
        {BankSize, undefined, On} = lists:foldl(fun(Battery, {Index, RemBank, AccOn}) ->
            FromIndex = erlang:max(0, BatteriesOnInBank + Index - BankSize),
            PlaceBatteryFun = fun
                PlaceBatteryFun(OnIndex) when OnIndex >= BatteriesOnInBank ->
                    AccOn;
                PlaceBatteryFun(OnIndex) ->
                    case maps:get(OnIndex, AccOn) of
                        {I, _} when I =:= Index ->
                            AccOn;
                        {I, AccBattery} when I < Index, Battery > AccBattery ->
                            GenerateOnFun(Index+1, OnIndex+1, AccOn#{OnIndex => {Index, Battery}}, RemBank);
                        {_, _} ->
                            PlaceBatteryFun(OnIndex+1)
                    end
            end,
            NewAccOn = PlaceBatteryFun(FromIndex),
            %ja_erl_utils_terminal:print("\t~p -> ~p [~p] -> ~p", [on_to_int(AccOn, BatteriesOnInBank), Battery, Index, on_to_int(NewAccOn, BatteriesOnInBank)]),
            %ja_erl_utils_terminal:print("\t~p -> ~p [~p] -> ~p", [AccOn, Battery, Index, NewAccOn]),
            NewRemBank = case RemBank of
                []             -> undefined;
                [_|RemRemBank] -> RemRemBank
            end,
            {Index+1, NewRemBank, NewAccOn}
        end, {0, erlang:tl(Bank), GenerateOnFun(0, 0, #{}, Bank)}, Bank),
        %ja_erl_utils_terminal:print("~p", [On]),
        Int = on_to_int(On, BatteriesOnInBank),
        %ja_erl_utils_terminal:print("~p", [Int]),
        Int
    end, Banks).


solve_1(FileName) ->
    solve(FileName, 2).

solve_2(FileName) ->
    solve(FileName, 12).
