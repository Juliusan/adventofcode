-module(day07).
-export([solve_1/1, solve_2/1]).

% Ir vėl užduotis nebuvo sunki. Darant pirmąją padariau žioplą klaidą listą
% be reikalo įdėdamas į listą. Tas kainavo 5-10 minučių kol išsiaiškinau.
% Šiaip viskas buvo tiesiai į tikslą. Šunkeliais nevaikščiojau. Ir visai grakštus
% sprendimas gavosi. Tiesa, antrą užduotį daro apie 1,5 sekundės.

% (aoc_2024@JuliusErisataT14.erisata.lt)1> day07:solve_1("priv/day07-PVZ.txt").
% 3749
% (aoc_2024@JuliusErisataT14.erisata.lt)2> day07:solve_1("priv/day07.txt").
% 303766880536
% (aoc_2024@JuliusErisataT14.erisata.lt)3> day07:solve_2("priv/day07-PVZ.txt").
% 11387
% (aoc_2024@JuliusErisataT14.erisata.lt)4> day07:solve_2("priv/day07.txt").
% 337041851384440


read_line(LineNoNL) ->
    {Result, [$:|LineNoNL2]} = utils:get_integer(LineNoNL),
    Numbers = utils:get_integer_list(LineNoNL2),
    {Result, Numbers}.


correct_result({Result, [Nr1|Numbers]}, DoConcat) ->
    AllResults = all_results([Nr1], Numbers, [], DoConcat),
    case lists:member(Result, AllResults) of
        true  -> Result;
        false -> 0
    end.


all_results(Nrs,      [],           _,   _       ) -> Nrs;
all_results([],       [_|Numbers],  Acc, DoConcat) -> all_results(Acc, Numbers, [], DoConcat);
all_results([Nr1|Nrs],[Nr2|Numbers],Acc, DoConcat) ->
    NewResults1 = [Nr1+Nr2,Nr1*Nr2],
    NewResults2 = case DoConcat of
        false -> NewResults1;
        true  -> [utils:concat_integers(Nr1, Nr2) | NewResults1]
    end,
    all_results(Nrs, [Nr2|Numbers], NewResults2 ++ Acc, DoConcat).


solve_1(FileName) ->
    Operations = utils:read_lines_no_new_line_to_elems(FileName, fun read_line/1),
    lists:sum(lists:map(fun(Operation) -> correct_result(Operation, false) end, Operations)).


solve_2(FileName) ->
    Operations = utils:read_lines_no_new_line_to_elems(FileName, fun read_line/1),
    lists:sum(lists:map(fun(Operation) -> correct_result(Operation, true) end, Operations)).
