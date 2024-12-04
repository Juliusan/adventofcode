-module(day02).
-export([solve_1/1, solve_2/1, solve_2_naive/1]).

% Pirma dalis buvo nesunki, o su antra užstrigau. Bandžiau per raportą eiti
% vieną kartą, ir kol išgaudžiau visas smulkmenas, užtruko laiko. Pasirodo, tai
% buvo "premature optimisation". Vėliau per kokias 10 minučių parašiau
% paprastesnę funkciją (solve_2_naive/1), kuri per raportą ėjo kelis kartus,
% bet raportai buvo neilgi. Tiesą, kad raportai neilgi, pastebėjau iš karto.
% Bet vis tiek visų pirma bandžiau optimaliai padaryti.

% (aoc_2024@JuliusErisataT14.erisata.lt)1> day02:solve_1("priv/day02-PVZ.txt").
% 2
% (aoc_2024@JuliusErisataT14.erisata.lt)2> day02:solve_1("priv/day02.txt").
% 257
% (aoc_2024@JuliusErisataT14.erisata.lt)3> day02:solve_2("priv/day02-PVZ.txt").
% 4
% (aoc_2024@JuliusErisataT14.erisata.lt)4> day02:solve_2("priv/day02.txt").
% 328


%%
%%  Checks if List is in strictly descending order with consecutive elements not further
%%  apart than Diff, or it would be if a single element would be removed.
%% 
is_decreasing_single_fail([],                  _   ) -> true;
is_decreasing_single_fail([_],                 _   ) -> true;
is_decreasing_single_fail([Elem1, Elem2|Else], Diff) -> is_decreasing_single_fail([Elem1,Elem2|Else], Diff, Elem2+1, false).

is_decreasing_single_fail([_], _, _, _) ->
    true;

is_decreasing_single_fail([Elem1, Elem2|Else], Diff, _Prev, Failed) when Elem1 > Elem2, Elem1-Elem2 =< Diff ->
    is_decreasing_single_fail([Elem2|Else], Diff, Elem1, Failed);

is_decreasing_single_fail([Elem1, Elem2|Else], Diff, Prev, false) ->
    case {is_decreasing_single_fail([Prev, Elem2|Else], Diff, undefined, true), Else} of
        {true, _} ->
            true;
        {false, []} ->
            true;
        {false, [Elem3|ElseElse]} ->
            is_decreasing_single_fail([Elem1, Elem3|ElseElse], Diff, Prev, true)
    end;

is_decreasing_single_fail(_, _, _, _) ->
    false.


%%
%%  Checks if List is in strictly ascending order with consecutive elements not further
%%  apart than Diff, or it would be if a single element would be removed.
%% 
is_increasing_single_fail([],                 _   ) -> true;
is_increasing_single_fail([_],                _   ) -> true;
is_increasing_single_fail([Elem1,Elem2|Else], Diff) -> is_increasing_single_fail([Elem1,Elem2|Else], Diff, Elem2-1, false).

is_increasing_single_fail([_], _, _, _) ->
    true;

is_increasing_single_fail([Elem1, Elem2|Else], Diff, _Prev, Failed) when Elem2 > Elem1, Elem2-Elem1 =< Diff ->
    is_increasing_single_fail([Elem2|Else], Diff, Elem1, Failed);

is_increasing_single_fail([Elem1, Elem2|Else], Diff, Prev, false) ->
    case {is_increasing_single_fail([Prev, Elem2|Else], Diff, undefined, true), Else} of
        {true, _} ->
            true;
        {false, []} ->
            true;
        {false, [Elem3|ElseElse]} ->
            is_increasing_single_fail([Elem1, Elem3|ElseElse], Diff, Prev, true)
    end;

is_increasing_single_fail(_, _, _, _) ->
    false.


solve_1(FileName) ->
    Reports = utils:read_line_to_elem(FileName, fun utils:get_integer_list/1),
    erlang:length(lists:filter(fun(Report) ->
        case utils:is_decreasing(Report, 3) of
            true  -> true;
            false -> utils:is_increasing(Report, 3)
        end
    end, Reports)).


solve_2(FileName) ->
    Reports = utils:read_line_to_elem(FileName, fun utils:get_integer_list/1),
    erlang:length(lists:filter(fun(Report) ->
        case is_decreasing_single_fail(Report, 3) of
            true  -> true;
            false -> is_increasing_single_fail(Report, 3)
        end
    end, Reports)).


solve_2_naive(FileName) ->
    Reports = utils:read_line_to_elem(FileName, fun utils:get_integer_list/1),
    DecreasingOrIncreasingFun = fun(Report) ->
        case utils:is_decreasing(Report, 3) of
            true  -> true;
            false -> utils:is_increasing(Report, 3)
        end
    end,
    erlang:length(lists:filter(fun(Report) ->
        ReportVariants = [Report | [ Start ++ End || N <- lists:seq(0, erlang:length(Report)), {Start, [_|End]} <- [lists:split(N, Report)] ] ],
        lists:any(DecreasingOrIncreasingFun, ReportVariants)
    end, Reports)).
    

%%% ============================================================================
%%% Test cases for internal functions.
%%% ============================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

is_decreasing_single_fail_test_() ->
    [
        ?_assertEqual(true,  is_decreasing_single_fail([],                   1)),
        ?_assertEqual(true,  is_decreasing_single_fail([20],                 1)),
        ?_assertEqual(true,  is_decreasing_single_fail([20, 19, 18, 17, 16], 1)),
        ?_assertEqual(false, is_decreasing_single_fail([20, 19, 18, 15, 12], 1)),
        ?_assertEqual(true,  is_decreasing_single_fail([20, 19, 18, 17, 16], 3)),
        ?_assertEqual(true,  is_decreasing_single_fail([20, 19, 18, 15, 12], 3)),
        ?_assertEqual(true,  is_decreasing_single_fail([15, 19, 18, 17, 16], 3)),
        ?_assertEqual(true,  is_decreasing_single_fail([20, 21, 19, 17, 15], 3)),
        ?_assertEqual(true,  is_decreasing_single_fail([20, 19, 21, 17, 16], 3)),
        ?_assertEqual(true,  is_decreasing_single_fail([20, 19, 18, 21, 16], 3)),
        ?_assertEqual(true,  is_decreasing_single_fail([20, 19, 18, 15, 21], 3)),
        ?_assertEqual(true,  is_decreasing_single_fail([20, 15, 14, 13, 12], 3)),
        ?_assertEqual(true,  is_decreasing_single_fail([20, 15, 19, 18, 15], 3)),
        ?_assertEqual(true,  is_decreasing_single_fail([20, 19, 15, 16, 15], 3)),
        ?_assertEqual(true,  is_decreasing_single_fail([20, 19, 16, 10, 15], 3)),
        ?_assertEqual(true,  is_decreasing_single_fail([20, 19, 18, 17, 12], 3)),
        ?_assertEqual(false, is_decreasing_single_fail([20, 15, 21, 17, 16], 3)),
        ?_assertEqual(false, is_decreasing_single_fail([20, 19, 21, 11, 10], 3)),
        ?_assertEqual(false, is_decreasing_single_fail([20, 19, 21, 18, 10], 3))
    ].


is_increasing_single_fail_test_() ->
    [
        ?_assertEqual(true,  is_increasing_single_fail([],                   1)),
        ?_assertEqual(true,  is_increasing_single_fail([20],                 1)),
        ?_assertEqual(true,  is_increasing_single_fail([20, 21, 22, 23, 24], 1)),
        ?_assertEqual(false, is_increasing_single_fail([20, 21, 22, 25, 28], 1)),
        ?_assertEqual(true,  is_increasing_single_fail([20, 21, 22, 23, 24], 3)),
        ?_assertEqual(true,  is_increasing_single_fail([20, 21, 22, 25, 28], 3)),
        ?_assertEqual(true,  is_increasing_single_fail([29, 21, 22, 25, 28], 3)),
        ?_assertEqual(true,  is_increasing_single_fail([20, 23, 22, 25, 28], 3)),
        ?_assertEqual(true,  is_increasing_single_fail([20, 23, 26, 25, 28], 3)),
        ?_assertEqual(true,  is_increasing_single_fail([20, 23, 26, 29, 28], 3)),
        ?_assertEqual(true,  is_increasing_single_fail([20, 23, 26, 27, 21], 3)),
        ?_assertEqual(true,  is_increasing_single_fail([10, 21, 22, 23, 26], 3)),
        ?_assertEqual(true,  is_increasing_single_fail([20, 24, 22, 23, 24], 3)),
        ?_assertEqual(true,  is_increasing_single_fail([20, 21, 25, 23, 24], 3)),
        ?_assertEqual(true,  is_increasing_single_fail([20, 21, 22, 26, 24], 3)),
        ?_assertEqual(true,  is_increasing_single_fail([20, 21, 22, 23, 34], 3)),
        ?_assertEqual(false, is_increasing_single_fail([20, 24, 18, 19, 21], 3)),
        ?_assertEqual(false, is_increasing_single_fail([20, 24, 25, 23, 26], 3)),
        ?_assertEqual(false, is_increasing_single_fail([20, 21, 19, 22, 29], 3))
    ].

-endif.