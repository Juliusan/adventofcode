-module(day01).
-export([solve_1/1, solve_2/1]).

% Užduotis nesunki, tik painiojo Erlanginiai modulis (`rem`) ir sveikoji dalis
% (`div`): neigiamiem skaičiams jie neigiami. Tai truputį pakomplikuoja kodą.
% Vėliau betvarkydamas kodą prisiminiau, kad savo bibliotekoje turiu pasidaręs
% euklidinius `rem` ir `div` variantus, tai panaudojau juos. Iš esmės niekas
% nepasikeitė tik kodas tapo gal kiek aiškesnis.

% (aoc_2025@JuliusErisataT14.erisata.lt)1> day01:solve_1("priv/day01-PVZ.txt").
% 3
% (aoc_2025@JuliusErisataT14.erisata.lt)2> day01:solve_1("priv/day01.txt").
% 1023
% (aoc_2025@JuliusErisataT14.erisata.lt)3> day01:solve_2("priv/day01-PVZ.txt").
% 6
% (aoc_2025@JuliusErisataT14.erisata.lt)4> day01:solve_2("priv/day01.txt").
% 5899
% (aoc_2025@JuliusErisataT14.erisata.lt)5> timer:tc(fun() -> day01:solve_1("priv/day01.txt") end).
% {21441,1023}
% (aoc_2025@JuliusErisataT14.erisata.lt)6> timer:tc(fun() -> day01:solve_2("priv/day01.txt") end).
% {11804,5899}

read(FileName) ->
    lists:reverse(ja_erl_utils_file:read_lines_no_new_line_to_elems(FileName, fun([D | Nr]) ->
        Direction = case D of
            $R -> right;
            $L -> left
        end,
        Number = erlang:list_to_integer(Nr),
        {Direction, Number}
    end)).


solve_1(FileName) ->
    Lines = read(FileName),
    %ja_erl_utils_terminal:print("~p", [Lines]),
    {Count, _LastPos} = ja_erl_utils_list:filter_count_foldl(fun({Direction, Number}, CurrPos) ->
        NewPos = ja_erl_utils_int:euclidean_rem(case Direction of
            right -> CurrPos + Number;
            left  -> CurrPos - Number
        end, 100),
        %ja_erl_utils_terminal:print("~p + ~p-~p -> ~p", [CurrPos, Direction, Number, NewPos]),
        case NewPos of
            0 -> {true, NewPos};
            _ -> {false, NewPos}
        end
    end, 50, Lines),
    Count.

solve_2(FileName) ->
    Lines = read(FileName),
    {Count, _LastPos} = ja_erl_utils_list:map_sum_foldl(fun({Direction, Number}, CurrPos) ->
        ResultPos = case Direction of
            right -> CurrPos + Number;
            left  -> CurrPos - Number
        end,
        NewPos = ja_erl_utils_int:euclidean_rem(ResultPos, 100),
        Clicks = abs(ja_erl_utils_int:euclidean_div(ResultPos, 100)),
        Correction = case {Direction, CurrPos, NewPos} of
            {left, 0, _} -> -1;
            {left, _, 0} -> 1;
            {_,    _, _} -> 0
        end,
        %ja_erl_utils_terminal:print("~p + ~p-~p -> ~p -> ~p", [CurrPos, Direction, Number, Clicks, NewPos]),
        {Clicks + Correction, NewPos}
    end, 50, Lines),
    Count.
