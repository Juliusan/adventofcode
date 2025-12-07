-module(utils).
-export([
    ja_erl_utils_list_multiply/1,
    ja_erl_utils_list_map_multiply/2,
    ja_erl_utils_matrix_get/2,
    ja_erl_utils_matrix_get_indices/3,
    ja_erl_utils_matrix_get_adjacent_indices/2,
    ja_erl_utils_matrix_get_neighbor_indices/2
]).

-include_lib("ja_erl_utils/include/ja_erl_utils_matrix.hrl").


ja_erl_utils_list_multiply(List) ->
    lists:foldl(fun(Elem, Acc) -> Elem*Acc end, 1, List).


ja_erl_utils_list_map_multiply(MapFun, List) ->
    lists:foldl(fun(Elem, Acc) -> MapFun(Elem)*Acc end, 1, List).


ja_erl_utils_matrix_get(Indices, Matrix) ->
    lists:map(fun(Index) -> ja_erl_utils_matrix:get(Index, Matrix) end, Indices).


ja_erl_utils_matrix_get_indices(Index, Directions, Matrix) ->
    lists:foldl(fun(Direction, AccIndices) ->
        case ja_erl_utils_matrix:next_index(Index, Direction, Matrix) of
            undefined -> AccIndices;
            NextIndex -> [NextIndex|AccIndices]
        end
    end, [], Directions).


ja_erl_utils_matrix_get_adjacent_indices(Index, Matrix) ->
    ja_erl_utils_matrix_get_indices(Index, ?MATRIX_DIRECTIONS_ALL, Matrix).


ja_erl_utils_matrix_get_neighbor_indices(Index, Matrix) ->
    {Row, Column} = Index,
    NextIndexes = [ {Row+R, Column+C} || R <- [-1, 0, 1], C <- [-1, 0, 1], R =/= 0 orelse C =/= 0],
    lists:filter(fun(I) ->
        ja_erl_utils_matrix:is_valid_index(I, Matrix)
    end, NextIndexes).
