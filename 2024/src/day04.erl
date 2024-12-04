-module(day04).
-export([solve_1/1, solve_2/1]).

% Pirma dalis buvo nesunki, užtrukau, kol parašiau istrižainių išrinkimo
% funkciją. Su indeksais būtų buvę paprasčiau, bet norėjau padaryti optimaliai.
% Antroje dalyje bandžiau visaip kaip panaudoti tą istrižainių išrinkimą, bet
% po 20 minučių supratau, kad bus lengviau parašyti visai atskirą algoritmą.
% Gavosi visai paprastas algoritmas, tik ne visai teisingai iš pirmo karto
% sudėliojau X-MAS kryžiuką per tris eilutes.

% (aoc_2024@JuliusErisataT14.erisata.lt)1> day04:solve_1("priv/day04-PVZ1.txt").
% 4
% (aoc_2024@JuliusErisataT14.erisata.lt)2> day04:solve_1("priv/day04-PVZ2.txt").
% 18
% (aoc_2024@JuliusErisataT14.erisata.lt)3> day04:solve_1("priv/day04.txt").
% 2427
% (aoc_2024@JuliusErisataT14.erisata.lt)4> day04:solve_2("priv/day04-PVZ2.txt").
% 9
% (aoc_2024@JuliusErisataT14.erisata.lt)5> day04:solve_2("priv/day04.txt").
% 1900


count_xmas_lines(Lines) ->
    lists:sum(lists:map(fun count_xmas/1, Lines)).

count_xmas(Line) ->
    count_xmas(Line, 0).

count_xmas([], Acc)                 -> Acc;
count_xmas([$X,$M,$A,$S|Else], Acc) -> count_xmas(Else, Acc+1);
count_xmas([_|Else], Acc) -> count_xmas(Else, Acc).


count_x_mas_lines(Lines) ->
    count_x_mas_lines(Lines, 0).

count_x_mas_lines([],                          Acc) -> Acc;
count_x_mas_lines([_],                         Acc) -> Acc;
count_x_mas_lines([_,     _],                  Acc) -> Acc;
count_x_mas_lines([Line1, Line2, Line3|Lines], Acc) ->
    Count = count_x_mas(Line1, Line2, Line3, 0),
    count_x_mas_lines([Line2, Line3|Lines], Acc+Count).

count_x_mas([],                    [],                   [],                    Acc) -> Acc;
count_x_mas([$M,E12,$M=E13|Line1], [_,$A=E22,E23|Line2], [$S,E32,$S=E33|Line3], Acc) -> count_x_mas([E12,E13|Line1], [E22,E23|Line2], [E32,E33|Line3], Acc + 1);
count_x_mas([$M,E12,$S=E13|Line1], [_,$A=E22,E23|Line2], [$M,E32,$S=E33|Line3], Acc) -> count_x_mas([E12,E13|Line1], [E22,E23|Line2], [E32,E33|Line3], Acc + 1);
count_x_mas([$S,E12,$M=E13|Line1], [_,$A=E22,E23|Line2], [$S,E32,$M=E33|Line3], Acc) -> count_x_mas([E12,E13|Line1], [E22,E23|Line2], [E32,E33|Line3], Acc + 1);
count_x_mas([$S,E12,$S=E13|Line1], [_,$A=E22,E23|Line2], [$M,E32,$M=E33|Line3], Acc) -> count_x_mas([E12,E13|Line1], [E22,E23|Line2], [E32,E33|Line3], Acc + 1);
count_x_mas([_|Line1],             [_|Line2],            [_|Line3],             Acc) -> count_x_mas(Line1,           Line2,           Line3,           Acc    ).


reverse_sublists(Lines) ->
    lists:map(fun lists:reverse/1, Lines).


solve_1(FileName) ->
    Lines    = utils:read_lines(FileName),
    LinesR   = reverse_sublists(Lines),
    LinesT   = utils:transpose(Lines),
    LinesTR  = reverse_sublists(LinesT),
    LinesDB  = utils:diagonals_b(Lines),
    LinesDBR = reverse_sublists(LinesDB),
    LinesDF  = utils:diagonals_b(LinesR),
    LinesDFR = reverse_sublists(LinesDF),
    Hor    = count_xmas_lines(Lines),
    HorR   = count_xmas_lines(LinesR),
    Ver    = count_xmas_lines(LinesT),
    VerR   = count_xmas_lines(LinesTR),
    DiagB  = count_xmas_lines(LinesDB),
    DiagBT = count_xmas_lines(LinesDBR),
    DiagF  = count_xmas_lines(LinesDF),
    DiagFT = count_xmas_lines(LinesDFR),
    % PrintLinesFun = fun(Liness) ->
    %     utils:print("VVVVVVVVVVVVVVVV", []),
    %     lists:foreach(fun(Line) -> utils:print("~s", [Line]) end, Liness),
    %     utils:print("AAAAAAAAAAAAAAAA", [])
    % end,
    % PrintLinesFun(Lines),
    % PrintLinesFun(LinesDB),
    % PrintLinesFun(LinesDBR),
    % PrintLinesFun(LinesDF),
    % PrintLinesFun(LinesDFR),
    % utils:print("Result: ~p ~p ~p ~p ~p ~p ~p ~p", [Hor, HorR, Ver, VerR, DiagB, DiagBT, DiagF, DiagFT]),
    Hor + HorR + Ver + VerR + DiagB + DiagBT + DiagF + DiagFT.


solve_2(FileName) ->
    Lines = utils:read_lines(FileName),
    count_x_mas_lines(Lines).
