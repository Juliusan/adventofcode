-module(day16_2).
-export([solve/1]).

% Nesudėtinga. Padariau paprastuoju būdu ir leidau suktis. Sekundė padauginus iš 440 nėra taip smarkiai daug,
% Be to kai kuriais atvejais žemėlapyje ilgai neužsibūdavo. Padariau optimizaciją, kuri smarkiai nieko
% neoptimizavo. Galutinė mano programos veikimo trukmė apie 230 sekundžių (3 min 50 s). Nors vėliau vakare
% prisėdau ir parašiau, kad perrašiau, kad tilptų į 3 sekundes.

solve(FileName) ->
    Map = get_map(FileName),
    {MapE, Size} = encode_map(Map),
    Result = get_best_traverse(MapE, Size),
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
            (Tile, {Column, AccME}) ->
                TileAtom = case Tile of
                    $.  -> empty;
                    $|  -> top_down;
                    $-  -> left_right;
                    $/  -> f_slash;
                    $\\ -> b_slash
                end,
                {Column+1, AccME#{Column => {{Row, Column}, TileAtom, []}}}
        end, {1, #{}}, MapLine),
        {Row+1, AccMapE#{Row => NewMapE}}
    end, {1, #{}}, Map),
    [First|_] = Map,
    Rows = Rows1-1,
    Cols = erlang:length(First),
    {MapE, {Rows, Cols}}.
    
    
get_best_traverse(Map, {Rows, Cols} = MapSize) ->
    %io:fwrite("Traversing map ~p~n", [MapSize]),
    TraverseFun = fun(Index, Direction, AccBest) ->
        %io:fwrite("  Entering ~p from ~p: ", [Index, Direction]),
        Count = traverse_and_count(Map, MapSize, Index, Direction),
        %io:fwrite("~p~n", [Count]),
        erlang:max(Count, AccBest)
    end,
    Best1 = lists:foldl(fun(Row, AccBest) -> TraverseFun({Row,1},    left,   AccBest) end, 0,     lists:seq(1, Rows)),
    Best2 = lists:foldl(fun(Row, AccBest) -> TraverseFun({Row,Cols}, right,  AccBest) end, Best1, lists:seq(1, Rows)),
    Best3 = lists:foldl(fun(Col, AccBest) -> TraverseFun({1,   Col}, top,    AccBest) end, Best2, lists:seq(1, Cols)),
    Best4 = lists:foldl(fun(Col, AccBest) -> TraverseFun({Rows,Col}, bottom, AccBest) end, Best3, lists:seq(1, Cols)),
    Best4.
    
    
traverse_and_count(Map, MapSize, Start, Direction) ->
    Traversed = traverse_map(Map, MapSize, Start, Direction),
    count_traversed(Traversed).

traverse_map(Map, _MapSize, {0,   _     }, _Direction)                    -> Map;
traverse_map(Map, _MapSize, {_,   0     }, _Direction)                    -> Map;
traverse_map(Map, {Rows,_}, {Row, _     }, _Direction) when Row > Rows    -> Map;
traverse_map(Map, {_,Cols}, {_,   Column}, _Direction) when Column > Cols -> Map;
traverse_map(Map, MapSize,  {Row, Column},  Direction) ->
    #{Row := RowMap = #{Column := {{Row, Column}, Type, Directions}}} = Map,
    IsTraversed = lists:member(Direction, Directions),
    NewMap = case IsTraversed of
        true  -> Map;
        false -> Map#{Row => RowMap#{Column => {{Row, Column}, Type, [Direction|Directions]}}}
    end,
    case IsTraversed of
        true ->
            NewMap;
        false ->
            case {Type, Direction} of
                {empty,      left  } ->           traverse_map(NewMap,  MapSize, {Row, Column+1}, left  );
                {empty,      top   } ->           traverse_map(NewMap,  MapSize, {Row+1, Column}, top   );
                {empty,      right } ->           traverse_map(NewMap,  MapSize, {Row, Column-1}, right );
                {empty,      bottom} ->           traverse_map(NewMap,  MapSize, {Row-1, Column}, bottom);
                {f_slash,    left  } ->           traverse_map(NewMap,  MapSize, {Row-1, Column}, bottom);
                {f_slash,    top   } ->           traverse_map(NewMap,  MapSize, {Row, Column-1}, right );
                {f_slash,    right } ->           traverse_map(NewMap,  MapSize, {Row+1, Column}, top   );
                {f_slash,    bottom} ->           traverse_map(NewMap,  MapSize, {Row, Column+1}, left  );
                {b_slash,    left  } ->           traverse_map(NewMap,  MapSize, {Row+1, Column}, top   );
                {b_slash,    top   } ->           traverse_map(NewMap,  MapSize, {Row, Column+1}, left  );
                {b_slash,    right } ->           traverse_map(NewMap,  MapSize, {Row-1, Column}, bottom);
                {b_slash,    bottom} ->           traverse_map(NewMap,  MapSize, {Row, Column-1}, right );
                {top_down,   top   } ->           traverse_map(NewMap,  MapSize, {Row+1, Column}, top   );
                {top_down,   bottom} ->           traverse_map(NewMap,  MapSize, {Row-1, Column}, bottom);
                {top_down,   _Other} -> NewMap2 = traverse_map(NewMap,  MapSize, {Row-1, Column}, bottom),
                                                  traverse_map(NewMap2, MapSize, {Row+1, Column}, top   );
                {left_right, left  } ->           traverse_map(NewMap,  MapSize, {Row, Column+1}, left  );
                {left_right, right } ->           traverse_map(NewMap,  MapSize, {Row, Column-1}, right );
                {left_right, _Other} -> NewMap2 = traverse_map(NewMap,  MapSize, {Row, Column-1}, right ),
                                                  traverse_map(NewMap2, MapSize, {Row, Column+1}, left  )
           end
   end.
            
    
count_traversed(Map) ->
    maps:fold(fun(_, MapRow, AccSum) ->
        maps:fold(fun
            (_, {_, _, []   }, AccS) -> AccS;
            (_, {_, _, [_|_]}, AccS) -> AccS+1
        end, AccSum, MapRow)
    end, 0, Map).


trim_ending_newline(Str) ->
    string:sub_string(Str, 1, string:len(Str) - 1).

