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


read(FileName) ->
    ja_erl_utils_file:read_lines_no_new_line_to_elems(FileName, fun(Line) ->
        lists:map(fun(Char) -> Char - $0 end, Line)
    end).


generate_init_on(Index, OnIndex, On, _RemBank, MaxIndex) when OnIndex >= MaxIndex ->
    On;

generate_init_on(Index, OnIndex, On, [Battery|RemBank], MaxIndex) ->
    generate_init_on(Index+1, OnIndex+1, On#{OnIndex => {Index, Battery}}, RemBank, MaxIndex).


place_batteries([], Index, AccOn, _BatteriesOnInBank, Index) ->
    AccOn;
place_batteries([Battery|RemBank], Index, AccOn, BatteriesOnInBank, BankSize) ->
    FromIndex = erlang:max(0, BatteriesOnInBank + Index - BankSize),
    NewAccOn = place_battery(Battery, RemBank, Index, FromIndex, AccOn, BatteriesOnInBank),
    %ja_erl_utils_terminal:print("\t~p -> ~p [~p] -> ~p", [on_to_int(AccOn, BatteriesOnInBank), Battery, Index, on_to_int(NewAccOn, BatteriesOnInBank)]),
    %ja_erl_utils_terminal:print("\t~p -> ~p [~p] -> ~p", [AccOn, Battery, Index, NewAccOn]),
    place_batteries(RemBank, Index+1, NewAccOn, BatteriesOnInBank, BankSize).


on_to_int(On, BatteriesOnInBank) ->
    lists:foldl(fun(Index, AccInt) ->
        {_Index, Battery} = maps:get(Index, On),
        AccInt * 10 + Battery
    end, 0, lists:seq(0, BatteriesOnInBank-1)).


place_battery(_Battery, _RemBank, _Index, OnIndex, On, BatteriesOnInBank) when OnIndex >= BatteriesOnInBank ->
    On;

place_battery(Battery, RemBank, Index, OnIndex, On, BatteriesOnInBank) ->
    case maps:get(OnIndex, On) of
        {I, _} when I =:= Index ->
            On;
        {I, AccBattery} when I < Index, Battery > AccBattery ->
            generate_init_on(Index+1, OnIndex+1, On#{OnIndex => {Index, Battery}}, RemBank, BatteriesOnInBank);
        {_, _} ->
            place_battery(Battery, RemBank, Index, OnIndex+1, On, BatteriesOnInBank)
    end.

solve_1(FileName) ->
    Banks = read(FileName),
    %ja_erl_utils_terminal:print("~p", [Banks]),
    BatteriesOnInBank = 2,
    ja_erl_utils_list:map_sum(fun(Bank) ->
        %ja_erl_utils_terminal:print("~p", [generate_init_on(0, #{}, Bank, BatteriesOnInBank)]),
        BankSize = erlang:length(Bank),
        On = place_batteries(Bank, 0, generate_init_on(0, 0, #{}, Bank, BatteriesOnInBank), BatteriesOnInBank, BankSize),
        %ja_erl_utils_terminal:print("~p", [On]),
        Int = lists:foldl(fun(Index, AccInt) ->
            {_Index, Battery} = maps:get(Index, On),
            AccInt * 10 + Battery
        end, 0, lists:seq(0, BatteriesOnInBank-1)),
        %ja_erl_utils_terminal:print("~p", [Int]),
        Int
    end, Banks).


solve_2(FileName) ->
    Banks = read(FileName),
    %ja_erl_utils_terminal:print("~p", [Banks]),
    BatteriesOnInBank = 12,
    ja_erl_utils_list:map_sum(fun(Bank) ->
        %ja_erl_utils_terminal:print("~p", [generate_init_on(0, #{}, Bank, BatteriesOnInBank)]),
        BankSize = erlang:length(Bank),
        On = place_batteries(Bank, 0, generate_init_on(0, 0, #{}, Bank, BatteriesOnInBank), BatteriesOnInBank, BankSize),
        %ja_erl_utils_terminal:print("~p", [On]),
        Int = lists:foldl(fun(Index, AccInt) ->
            {_Index, Battery} = maps:get(Index, On),
            AccInt * 10 + Battery
        end, 0, lists:seq(0, BatteriesOnInBank-1)),
        %ja_erl_utils_terminal:print("~p", [Int]),
        Int
    end, Banks).
