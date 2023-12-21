-module(day17_1).
-export([solve/1]).
-compile(export_all).

% Strigau prie šitos siaubingai. Baigiau tik 20 dieną. Parašiau gal 3 ar 4 skirtingus algoritmus (visus, žinoma,
% neteisingus). Galiausiai Rimas užvedė ant kelio su Dijkstra. Pasidariau patobulintą jo versiją ir viskas praėjo.

solve(FileName) ->
    Map = get_map(FileName),
    {MapE, Size} = encode_map(Map),
    Result = traverse_and_count(MapE, Size),
    Result.
    
    
get_map(FileName) ->
    {ok, File} = file:open(FileName, [read]),
    Map = get_map(File, []),
    ok = file:close(File),
    Map.
    
get_map(File, AccMap) ->
    case file:read_line(File) of
        eof ->
            lists:reverse(AccMap);
        {ok, Line} ->
            %io:fwrite("XXX ~p: ", [Line]),
            MapLine = trim_ending_newline(Line),
            %io:fwrite("~p~n", [MapNode]),
            get_map(File, [MapLine | AccMap])
    end.
    
    
encode_map(Map) ->
    {Rows1, MapE} = lists:foldl(fun(MapLine, {Row, AccMapE}) ->
        {_, NewMapE} = lists:foldl(fun
            (Digit, {Column, AccME}) ->
                {Column+1, AccME#{Column => {{Row, Column}, Digit - $0, undefined, [], []}}}
        end, {1, #{}}, MapLine),
        {Row+1, AccMapE#{Row => NewMapE}}
    end, {1, #{}}, Map),
    [First|_] = Map,
    Rows = Rows1-1,
    Cols = erlang:length(First),
    {MapE, {Rows, Cols}}.


traverse_and_count(Map, MapSize) ->
    {ok, {{1,1}, Loss, undefined, [], []}} = get_cell({1,1}, Map),
    IMap = fill_cell({1,1}, {{1,1}, Loss, 0, [{0, undefined}], []}, Map),
    Length = traverse_map(IMap, MapSize, [{0, {1, 1}}]),
    Length.

    
traverse_map(_Map, MapSize, [{Length, MapSize}|_]) ->
    Length;
    
traverse_map(Map, MapSize, [{PathLosses, Index}|Cells]) ->
    %print_map(Map),
    %io:fwrite("XXX ~p -> ~p (~p):~n", [Index, PathLosses, Cells]),
    %io:fread("", ""),
    {ok, {Index, Loss, Losses, Paths, Closed}} = get_cell(Index, Map),
    [{PathLosses, From}|OtherPaths] = Paths,
    NewMap1 = fill_cell(Index, {Index, Loss, Losses, OtherPaths, [From|Closed]}, Map),
    NewCells1 = case OtherPaths of
        []                  -> Cells;
        [{NextLosses, _}|_] -> insert({NextLosses, Index}, Cells)
    end,
    NextPaths = get_next_paths(MapSize, Index, From),
    %io:fwrite("  XXX NEXT ~p~n", [NextPaths]),
    {NewMap2, NewCells2} = mark_paths(NewMap1, NewCells1, PathLosses, NextPaths),
    %io:fwrite("  XXX NEW ~p~n", [NewCells2]),
    traverse_map(NewMap2, MapSize, NewCells2).
    
    
insert({From, _} = Elem, [{CFrom, _} = CElem|Cells]) when From > CFrom -> [CElem | insert(Elem, Cells)];
insert(            Elem,                     Cells )                   -> [Elem | Cells].


get_next_paths(_MapSize, {1, 1} = Index, undefined) ->  % Start
    [{{1, 2}, Index}, {{2, 1}, Index}];
    
get_next_paths(MapSize, {_, Col} = Index, {_, ColF} = From) when Col < ColF ->  % Left
    Down = get_next_path_down(MapSize, Index, From),
    Left = get_next_path_left(         Index, From),
    Up   = get_next_path_up(           Index, From),
    Down ++ Left ++ Up;
    
get_next_paths(MapSize, {Row, _} = Index, {RowF, _} = From) when Row < RowF ->  % Up
    Right = get_next_path_right(MapSize, Index, From),
    Up    = get_next_path_up(            Index, From),
    Left  = get_next_path_left(          Index, From),
    Right ++ Up ++ Left;
    
get_next_paths(MapSize, {_, Col} = Index, {_, ColF} = From) when Col > ColF ->  % Right
    Down  = get_next_path_down( MapSize, Index, From),
    Right = get_next_path_right(MapSize, Index, From),
    Up    = get_next_path_up(            Index, From),
    Down ++ Right ++ Up;
    
get_next_paths(MapSize, {Row, _} = Index, {RowF, _} = From) when Row > RowF ->  % Down
    Right = get_next_path_right(MapSize, Index, From),
    Down  = get_next_path_down( MapSize, Index, From),
    Left  = get_next_path_left(          Index, From),
    Right ++ Down ++ Left.
    
    
get_next_path_left({_, 1}, _)                                         -> [];
get_next_path_left({Row, Col}, {_RowF, Col})                          -> [{{Row,Col-1},{Row,Col}}];
get_next_path_left({Row, Col}, { Row, ColF}) when abs(Col-ColF) =:= 3 -> [];
get_next_path_left({Row, Col}, { Row, ColF}) when abs(Col-ColF)  <  3 -> [{{Row,Col-1},{Row,ColF}}].
    
get_next_path_up({1, _}, _)                                         -> [];
get_next_path_up({Row, Col}, {Row, _ColF})                          -> [{{Row-1,Col},{Row,Col}}];
get_next_path_up({Row, Col}, {RowF,  Col}) when abs(Row-RowF) =:= 3 -> [];
get_next_path_up({Row, Col}, {RowF,  Col}) when abs(Row-RowF)  <  3 -> [{{Row-1,Col},{RowF,Col}}].
    
get_next_path_right({_, Cols}, {_,  Cols}, _)                                     -> [];
get_next_path_right({_,    _}, {Row, Col}, {_RowF, Col})                          -> [{{Row,Col+1},{Row,Col}}];
get_next_path_right({_,    _}, {Row, Col}, { Row, ColF}) when abs(Col-ColF) =:= 3 -> [];
get_next_path_right({_,    _}, {Row, Col}, { Row, ColF}) when abs(Col-ColF)  <  3 -> [{{Row,Col+1},{Row,ColF}}].
    
get_next_path_down({Rows, _}, {Rows,  _}, _)                                     -> [];
get_next_path_down({_,    _}, {Row, Col}, {Row, _ColF})                          -> [{{Row+1,Col},{Row,Col}}];
get_next_path_down({_,    _}, {Row, Col}, {RowF,  Col}) when abs(Row-RowF) =:= 3 -> [];
get_next_path_down({_,    _}, {Row, Col}, {RowF,  Col}) when abs(Row-RowF)  <  3 -> [{{Row+1,Col},{RowF,Col}}].

    
mark_paths(Map, Cells, _CellLosses, []) -> {Map, Cells};
mark_paths(Map, Cells,  CellLosses, [{To, From}|Paths]) ->
    {ok, {Index, Loss, Losses, CellPaths, Closed}} = get_cell(To, Map),
    %io:fwrite("  XXX MARKING ~p -> ~p; ~p ~p ~p~n", [From, To, CellPaths, Closed, Cells]),
    case lists:member(From, Closed) of
        true ->
            mark_paths(Map, Cells, CellLosses, Paths);
        false ->
            PathLosses = CellLosses + Loss,
            NewLosses = case PathLosses < Losses of
                true  -> PathLosses;
                false -> Losses
            end,
            NewCellPaths = case lists:keytake(From, 2, CellPaths) of
                {value, { OldPathLosses, From},  OtherCellPaths} when PathLosses < OldPathLosses -> lists:sort([{PathLosses, From}|OtherCellPaths]);
                {value, {_OldPathLosses, From}, _OtherCellPaths}                                 -> CellPaths;
                false                                                                            -> lists:sort([{PathLosses, From}|CellPaths])
            end,
            NewMap = fill_cell(To, {To, Loss, NewLosses, NewCellPaths, Closed}, Map),
            NewCells = case NewCellPaths =:= CellPaths of
                true ->
                    Cells;
                false ->
                    OtherCells = case lists:keytake(Index, 2, Cells) of
                        {value, {_, Index}, OCells} -> OCells;
                        false                       -> Cells
                    end,
                    [{FirstLosses, _}|_] = NewCellPaths,
                    %io:fwrite("    XXX INSERT ~p -> ~p = ~p~n", [{FirstLosses, Index}, OtherCells, insert({FirstLosses, Index}, OtherCells)]),
                    insert({FirstLosses, Index}, OtherCells)
            end,
            mark_paths(NewMap, NewCells, CellLosses, Paths)
    end.
    
    
fill_cell({Row, Col}, With, AccMap) ->
    #{Row := RowMap} = AccMap,
    NewRowMap = RowMap#{Col => With},
    AccMap#{Row => NewRowMap}.
    
    
get_cell({Row, Col}, Map) ->
    case maps:find(Row, Map) of
        {ok, RowMap} ->
            case maps:find(Col, RowMap) of
                {ok, Cell} -> {ok, Cell};
                error      -> false
            end;
        error ->
            false
    end.
  
    
print_map(Map) ->
    MapList = lists:sort(maps:to_list(Map)),
    lists:foreach(fun({_, LineMap}) ->
        LineMapList = lists:sort(maps:to_list(LineMap)),
        lists:foreach(fun
            ({_, {_, _, undefined, _, _}}) -> io:fwrite("--- ");
            ({_, {_, _, Losses,    _, _}}) -> io:fwrite("~3.B ", [Losses])
        end, LineMapList),
        io:fwrite("~n")
    end, MapList).


trim_ending_newline(Str) ->
    string:sub_string(Str, 1, string:len(Str) - 1).

