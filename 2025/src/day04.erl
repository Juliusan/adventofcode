-module(day04).
-export([solve_1/1, solve_2/1]).

% Buvo lengviausias iš kol kas pasitaikiusių. Iš pradžių bandžiau galvoti
% gudresnį algoritmą, bet paskaičiavau, kad 138*138 langelių nėra tiek jau
% daug, ir nusprendžiau iš karto nekomplikuoti sau gyvenimo. Nebent vėliau.
% Tas vėliau atėjo su antra dalimi. Pagalvojau, kad daug kartų eiti per tuos
% pačius ~20 tūkstančių langelių gali būti ir daugoka. Nusprendžiau pasidaryti
% ir pakeliui palaikyti tik rulonų indeksų sąrašą. Nežinau, ar pasiteisino
% būtent tai, bet programa prasisuko. Tiesa, pirmą kartą šiais metais prieš
% išvesdama atsakymą trumpai žagtelėjo.

% (aoc_2025@JuliusErisataT14.erisata.lt)1> day04:solve_1("priv/day04-PVZ.txt").
% 13
% (aoc_2025@JuliusErisataT14.erisata.lt)2> day04:solve_1("priv/day04.txt").
% 1376
% (aoc_2025@JuliusErisataT14.erisata.lt)3> day04:solve_2("priv/day04-PVZ.txt").
% 43
% (aoc_2025@JuliusErisataT14.erisata.lt)4> day04:solve_2("priv/day04.txt").
% 8587
% (aoc_2025@JuliusErisataT14.erisata.lt)5> timer:tc(fun() -> day04:solve_1("priv/day04.txt") end).
% {78219,1376}
% (aoc_2025@JuliusErisataT14.erisata.lt)6> timer:tc(fun() -> day04:solve_2("priv/day04.txt") end).
% {346478,8587}


read(FileName) ->
    Lines = lists:reverse(ja_erl_utils_file:read_lines_no_new_line(FileName)),
    Matrix = ja_erl_utils_matrix:get_char_matrix(Lines),
    %ja_erl_utils_matrix:print_char_matrix(Matrix),
    Rolls = ja_erl_utils_matrix:foldf(fun
        (_,     $., AccIn) -> AccIn;
        (Index, $@, AccIn) -> [Index|AccIn]
    end, [], Matrix),
    {Matrix, Rolls}.


remove_rolls(RollIndexes, Matrix) ->
    {RollsRemoved, RollsRemaining} = lists:foldl(fun(RollIndex, {AccRemoved, AccRemaining}) ->
        ValidIndexes = utils:ja_erl_utils_matrix_get_neighbor_indices(RollIndex, Matrix),
        case erlang:length(ValidIndexes) of
            L when L <  4 ->
                {[RollIndex|AccRemoved], AccRemaining};
            L when L =< 8 ->
                Rolls = ja_erl_utils_list:filter_count(fun(Index) ->
                    Roll = ja_erl_utils_matrix:get(Index, Matrix),
                    Roll =:= $@
                end, ValidIndexes),
                case Rolls < 4 of
                    true  -> {[RollIndex|AccRemoved], AccRemaining};
                    false -> {AccRemoved, [RollIndex|AccRemaining]}
                end
        end
    end, {[], []}, RollIndexes),
    NewMatrix = lists:foldl(fun(Index, AccMatrix) ->
        ja_erl_utils_matrix:set(Index, AccMatrix, $.) end,
    Matrix, RollsRemoved),
    {RollsRemoved, RollsRemaining, NewMatrix}.


solve_1(FileName) ->
    {Matrix, Rolls} = read(FileName),
    {RollsRemoved, _, _} = remove_rolls(Rolls, Matrix),
    erlang:length(RollsRemoved).


solve_2(FileName) ->
    {Matrix, Rolls} = read(FileName),
    RemoveAllRollsFun = fun RemoveAllRollsFun(AccRollIndexes, AccMatrix, Count) ->
        {RollsRemoved, RollsRemaining, NewAccMatrix} = remove_rolls(AccRollIndexes, AccMatrix),
        case RollsRemoved of
            []    -> Count;
            [_|_] -> RemoveAllRollsFun(RollsRemaining, NewAccMatrix, Count + erlang:length(RollsRemoved))
        end
    end,
    RemoveAllRollsFun(Rolls, Matrix, 0).
