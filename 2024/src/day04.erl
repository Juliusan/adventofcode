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


count_x_mas(Lines) ->
    count_x_mas(Lines, 0).

count_x_mas([], Acc) -> Acc;
count_x_mas([_], Acc) -> Acc;
count_x_mas([_,_], Acc) -> Acc;
count_x_mas([Diag1, Diag2, Diag3|Else], Acc) ->
    Count = count_x_mas(Diag1, Diag2, Diag3, 0),
    count_x_mas([Diag2, Diag3|Else], Acc+Count).

count_x_mas([],[],[], Acc) -> Acc;
count_x_mas([$M,E1,$M|Else1],[_,$A,E2|Else2],[$S,E3,$S|Else3], Acc) -> count_x_mas([E1,$M|Else1],[$A,E2|Else2],[E3,$S|Else3], Acc + 1);
count_x_mas([$M,E1,$S|Else1],[_,$A,E2|Else2],[$M,E3,$S|Else3], Acc) -> count_x_mas([E1,$S|Else1],[$A,E2|Else2],[E3,$S|Else3], Acc + 1);
count_x_mas([$S,E1,$M|Else1],[_,$A,E2|Else2],[$S,E3,$M|Else3], Acc) -> count_x_mas([E1,$M|Else1],[$A,E2|Else2],[E3,$M|Else3], Acc + 1);
count_x_mas([$S,E1,$S|Else1],[_,$A,E2|Else2],[$M,E3,$M|Else3], Acc) -> count_x_mas([E1,$S|Else1],[$A,E2|Else2],[E3,$M|Else3], Acc + 1);
count_x_mas([_|Else1],[_|Else2],[_|Else3], Acc) -> count_x_mas(Else1,Else2,Else3, Acc).
    

-spec transpose(
    ListOfLists :: [list()]
) ->
    ListOfLists :: [list()].

transpose([[]|_]) ->
    [];
transpose(M) ->
    [lists:map(fun erlang:hd/1, M) | transpose(lists:map(fun erlang:tl/1, M))].


reverse_list(Lines) ->
    lists:map(fun lists:reverse/1, Lines).


diag_back(Lines) ->
    diag_back(Lines, 1, []).


diag_back([], _, Acc) -> Acc;
diag_back(Lines, Count, Acc) ->
    {Single, NewLines} = diag_back_single(Lines, Count, [], []),
    NewLines2 = lists:filter(fun([]) -> false; (_) -> true end, NewLines),
    %utils:print("BACK ~p ~p", [Single, NewLines2]),
    diag_back(NewLines2, Count+1, [Single|Acc]).

diag_back_single(ElseLines, 0, AccSingle, AccLines) -> {AccSingle, lists:reverse(AccLines)++ElseLines};
diag_back_single([], _, AccSingle, AccLines) -> {AccSingle, lists:reverse(AccLines)};
diag_back_single([[]|ElseLines], _, AccSingle, AccLines) -> {AccSingle, lists:reverse(AccLines) ++ ElseLines};
diag_back_single([[Elem|Else]|ElseLines], Curr, AccSingle, AccLines) -> 
    diag_back_single(ElseLines, Curr-1, [Elem|AccSingle], [Else|AccLines]).

solve_1(FileName) ->
    Lines    = utils:read_lines(FileName),
    LinesR   = reverse_list(Lines),
    LinesT   = transpose(Lines),
    LinesTR  = reverse_list(LinesT),
    LinesDB  = diag_back(Lines),
    LinesDBR = reverse_list(LinesDB),
    LinesDF  = diag_back(LinesR),
    LinesDFR = reverse_list(LinesDF),
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
    count_x_mas(Lines).


%%% ============================================================================
%%% Test cases for internal functions.
%%% ============================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

diag_back_test_() ->
    [
        ?_assertEqual([[4],[3,2],[1]],  diag_back([[1,2],[3,4]])),
        ?_assertEqual([[6],[5,3],[4,2],[1]],  diag_back([[1,2,3],[4,5,6]])),
        ?_assertEqual([[6],[5,4],[3,2],[1]],  diag_back([[1,2],[3,4],[5,6]])),
        ?_assertEqual([[9],[8,6],[7,5,3],[4,2],[1]],  diag_back([[1,2,3],[4,5,6],[7,8,9]])),
        ?_assertEqual(ok, ok)
    ].


-endif.