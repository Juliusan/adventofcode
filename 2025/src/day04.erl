-module(day04).
-export([solve_1/1, solve_2/1]).


find_rolls(Matrix) ->
    ja_erl_utils_matrix:foldf(fun
        (_,     $., AccIn) -> AccIn;
        (Index, $@, AccIn) -> [Index|AccIn]
    end, [], Matrix).


remove_rolls(RollIndexes, Matrix) ->
    {RollsRemoved, RollsRemaining} = lists:foldl(fun({Row, Column} = RollIndex, {AccRemoved, AccRemaining}) ->
        Indexes = [ {Row+R, Column+C} || R <- [-1, 0, 1], C <- [-1, 0, 1]] -- [{Row, Column}],
        ValidIndexes = lists:filter(fun(Index) ->
            ja_erl_utils_matrix:is_valid_index(Index, Matrix)
        end, Indexes),
        case erlang:length(Indexes) of
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
    Lines = lists:reverse(ja_erl_utils_file:read_lines_no_new_line(FileName)),
    Matrix = ja_erl_utils_matrix:get_char_matrix(Lines),
    Rolls = find_rolls(Matrix),
    {RollsRemoved, _, _} = remove_rolls(Rolls, Matrix),
    erlang:length(RollsRemoved).


solve_2(FileName) ->
    Lines = lists:reverse(ja_erl_utils_file:read_lines_no_new_line(FileName)),
    Matrix = ja_erl_utils_matrix:get_char_matrix(Lines),
    %ja_erl_utils_matrix:print_char_matrix(Matrix),
    Rolls = find_rolls(Matrix),
    RemoveAllRollsFun = fun RemoveAllRollsFun(AccRollIndexes, AccMatrix, Count) ->
        {RollsRemoved, RollsRemaining, NewAccMatrix} = remove_rolls(AccRollIndexes, AccMatrix),
        case RollsRemoved of
            []    -> Count;
            [_|_] -> RemoveAllRollsFun(RollsRemaining, NewAccMatrix, Count + erlang:length(RollsRemoved))
        end
    end,
    RemoveAllRollsFun(Rolls, Matrix, 0).
