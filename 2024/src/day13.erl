-module(day13).
-export([solve_1/1, solve_2/1]).
-export([solve/1]).

% Toks lengvas, o beveik neturėjau laiko prisėsti. Pradžioje padariau žioplą
% klaidą ir niekaip negalėjau jos ištaisyti. Tai trukau ilgiau, negu turėjau.
% Bet už tai antrai daliai 4 minučių man nereikėjo.

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
    BX2 = BX*AY-BY*AX,
    PX2 = PX*AY-PY*AX,
    Correction = case {BX2, PX2} of
        {N, M} when N>0, M>0 -> {BX2, PX2};
        {N, M} when N<0, M<0 -> {-BX2, -PX2};
        _ -> undefined
    end,
    case Correction of
        {BX3, PX3} ->
            case PX3 rem BX3 of
                0 ->
                    B = PX3 div BX3,
                    PX4 = PX - BX*B,
                    case PX4 rem AX of
                        0 ->
                            A = PX4 div AX,
                            PY = AY*A + BY*B,
                            {A, B};
                        _ ->
                            {0, 0}
                    end;
                _ ->
                    {0, 0}
            end;
        undefined ->
            {0, 0}
    end.


solve_all(List) ->
    lists:foldl(fun(Elem, Acc) ->
        {A, B} = solve(Elem),
        Acc + A*3 + B
    end, 0, List).


solve_1(FileName) ->
    List = read_inputs(FileName),
    solve_all(List).


solve_2(FileName) ->
    List = read_inputs(FileName),
    List2 = lists:map(fun({AX, AY, BX, BY, PX, PY}) -> {AX, AY, BX, BY, PX+10000000000000, PY+10000000000000} end, List),
    solve_all(List2).
