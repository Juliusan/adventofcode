-module(day13).
-export([solve_1/1, solve_2/1]).
-export([solve/1]).

% Toks lengvas, matematinis, o beveik neturėjau laiko prisėsti. Pradžioje
% padariau žioplą klaidą ir niekaip negalėjau jos ištaisyti. Tai trukau ilgiau,
% negu turėjau. Bet už tai antrai daliai 4 minučių man nereikėjo.

% (aoc_2024@JuliusErisataT14.erisata.lt)1> day13:solve_1("priv/day13-PVZ.txt").
% 480
% (aoc_2024@JuliusErisataT14.erisata.lt)2> day13:solve_1("priv/day13.txt").
% 35082
% (aoc_2024@JuliusErisataT14.erisata.lt)3> day13:solve_2("priv/day13-PVZ.txt").
% 875318608908
% (aoc_2024@JuliusErisataT14.erisata.lt)4> day13:solve_2("priv/day13.txt").
% 82570698600470
% (aoc_2024@JuliusErisataT14.erisata.lt)5> timer:tc(fun() -> day13:solve_1("priv/day13.txt") end).
% {9489,35082}
% (aoc_2024@JuliusErisataT14.erisata.lt)6> timer:tc(fun() -> day13:solve_2("priv/day13.txt") end).
% {9967,82570698600470}


read_inputs(FileName) ->
    Lines = utils:read_lines_no_new_line(FileName),
    read_input(Lines, []).


read_input([], Acc) -> Acc;
read_input([""|Lines], Acc) -> read_input(Lines, Acc);
read_input([Line1,Line2,Line3|Lines], Acc) ->
    {PX, PY} = read_prize(Line1),
    {BX, BY} = read_button($B, Line2),
    {AX, AY} = read_button($A, Line3),
    read_input(Lines, [{AX, AY, BX, BY, PX, PY}|Acc]).


read_button(Name, Line) ->
    "Button " ++ Line1 = Line,
    [Name|Line2] = Line1,
    ": X+" ++ Line3 = Line2,
    {IntX, ", Y+" ++ Line4} = utils:get_integer(Line3),
    {IntY, ""} = utils:get_integer(Line4),
    {IntX, IntY}.


read_prize(Line) ->
    "Prize: X=" ++ Line1 = Line,
    {IntX, ", Y=" ++ Line2} = utils:get_integer(Line1),
    {IntY, ""} = utils:get_integer(Line2),
    {IntX, IntY}.


solve({AX, AY, BX, BY, PX, PY}) ->
    case utils:solve_two_equations_int({AX, BX, PX}, {AY, BY, PY}) of
        {A, B} when is_integer(A), is_integer(B), A>=0, B>=0 -> {A, B};
        _                                                    -> undefined
    end.


solve_all(List) ->
    utils:list_map_sum(fun(Elem) ->
        case solve(Elem) of
            {A, B}    -> A*3 + B;
            undefined -> 0
        end
    end, List).


solve_1(FileName) ->
    List = read_inputs(FileName),
    solve_all(List).


solve_2(FileName) ->
    List = read_inputs(FileName),
    List2 = lists:map(fun({AX, AY, BX, BY, PX, PY}) -> {AX, AY, BX, BY, PX+10000000000000, PY+10000000000000} end, List),
    solve_all(List2).
