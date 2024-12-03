-module(day03).
-export([solve_1/1, solve_2/1]).

% Pirma dalis buvo nesunki, užtrukau, kol parašiau apytvarkingį sintaksinį
% analizatorių. Galvojau, tvarka pravers antroje dalyje. Ir iš tiesų pravertė.
% Antra dalis taip pat nebuvo sunki ir būčiau padaręs dar greičiau, jei iš
% karto būčiau sugalvojęs, kad vienos eilutės "don't" gali turėti įtakos kitai
% eilutei. Na, o po to per ilgai strigau prie elementaraus dalyko: šitoje
% programoje eilučių tvarka svarbi, o mano skaitymas grąžina jas apverstas.

% (aoc_2024@JuliusErisataT14.erisata.lt)1> day03:solve_1("priv/day03_1-PVZ.txt").
% 161
% (aoc_2024@JuliusErisataT14.erisata.lt)2> day03:solve_1("priv/day03.txt").
% 159833790
% (aoc_2024@JuliusErisataT14.erisata.lt)3> day03:solve_2("priv/day03_2-PVZ.txt").
% 48
% (aoc_2024@JuliusErisataT14.erisata.lt)4> day03:solve_2("priv/day03.txt").
% 89349241


parse_operation(Line) -> parse_operation(Line, false).

parse_operation(Line, ScanDos) -> parse_operation(Line, 0, true, ScanDos).

parse_operation("", Acc, _, _) ->
    Acc;

parse_operation([$m, $u, $l | Else], Acc, true, ScanDos) ->
    parse_param_list(Else, Acc, ScanDos);

parse_operation([$d, $o, $(, $) | Else], Acc, _, true) ->
    %utils:print("DO", []),
    parse_operation(Else, Acc, true, true);

parse_operation([$d, $o, $n, $', $t, $(, $) | Else], Acc, _, true) ->
    %utils:print("NOT", []),
    parse_operation(Else, Acc, false, true);
        
parse_operation([_ | Else], Acc, Enabled, ScanDos) ->
    parse_operation(Else, Acc, Enabled, ScanDos).


parse_param_list([$( | Else], Acc, ScanDos) ->
    case parse_number(Else) of
        {Nr1, Else1} ->
            case Else1 of
                [$, | Else2] ->
                    case parse_number(Else2) of
                        {Nr2, Else3} ->
                            case Else3 of
                                [$) | Else4] -> 
                                    NewAcc = Acc + Nr1*Nr2,
                                    %utils:print("MUL ~p ~p", [Nr1, Nr2]),
                                    parse_operation(Else4, NewAcc, true, ScanDos);
                                _ ->
                                    parse_operation(Else, Acc, true, ScanDos)
                            end;
                        error ->
                            parse_operation(Else, Acc, true, ScanDos)
                    end;
                _ ->
                    parse_operation(Else, Acc, true, ScanDos)
            end;
        error ->
            parse_operation(Else, Acc, true, ScanDos)
    end;

parse_param_list([_ | Else], Acc, ScanDos) ->
    parse_operation(Else, Acc, true, ScanDos).


parse_number([Digit|Else]) when $0 =< Digit, Digit =< $9 -> parse_number(Else, Digit-$0, 2);
parse_number(_) -> error.

parse_number(Line, Acc, 0) -> {Acc, Line};
parse_number([Digit|Else], Acc, Rem) when $0 =< Digit, Digit =< $9 -> parse_number(Else, Acc*10+Digit-$0, Rem-1);
parse_number(Line, Acc, _) -> {Acc, Line}.
    


solve_1(FileName) ->
    Memory = utils:read_file(FileName),
    parse_operation(Memory).


solve_2(FileName) ->
    Memory = utils:read_file(FileName),
    parse_operation(Memory, true).
