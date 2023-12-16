-module(day16_2).
-export([solve/1]).

% Nesudėtinga. Padariau paprastuoju būdu ir leidau suktis. Sekundė padauginus iš 440 nėra taip smarkiai daug,
% Be to kai kuriais atvejais žemėlapyje ilgai neužsibūdavo. Padariau optimizaciją, kuri smarkiai nieko
% neoptimizavo. Galutinė mano programos veikimo trukmė apie 230 sekundžių (3 min 50 s).

solve(FileName) ->
    Map = get_map(FileName),
    {Mirrors, Size} = get_mirrors(Map),
    Result = get_best_traverse(Mirrors, Size),
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
    
    
get_mirrors(Map) ->
    {Rows1, Mirrors} = lists:foldl(fun(MapLine, {Row, AccMirrors}) ->
        {_, NewMirrors} = lists:foldl(fun
            ($., {Column, AccMs}) ->
                {Column+1, AccMs};
            (Mirror, {Column, AccMs}) ->
                MirrorAtom = case Mirror of
                    $|  -> top_down;
                    $-  -> left_right;
                    $/  -> f_slash;
                    $\\ -> b_slash
                end,
                NewM = {Column, MirrorAtom},
                {Column+1, [NewM | AccMs]}
        end, {1, []}, MapLine),
        {Row+1, [{Row, NewMirrors}|AccMirrors]}
    end, {1, []}, Map),
    [First|_] = Map,
    Rows = Rows1-1,
    Cols = erlang:length(First),
    {Mirrors, {Rows, Cols}}.
    
    
get_best_traverse(Mirrors, {Rows, Cols} = MapSize) ->
    io:fwrite("Traversing map ~p~n", [MapSize]),
    TraverseFun = fun(Index, Direction, AccBest) ->
        io:fwrite("  Entering ~p from ~p: ", [Index, Direction]),
        Count = traverse_and_count(Mirrors, MapSize, Index, Direction),
        io:fwrite("~p~n", [Count]),
        erlang:max(Count, AccBest)
    end,
    Best1 = lists:foldl(fun(Row, AccBest) -> TraverseFun({Row,1},    left,   AccBest) end, 0,     lists:seq(1, Rows)),
    Best2 = lists:foldl(fun(Row, AccBest) -> TraverseFun({Row,Cols}, right,  AccBest) end, Best1, lists:seq(1, Rows)),
    Best3 = lists:foldl(fun(Col, AccBest) -> TraverseFun({1,   Col}, top,    AccBest) end, Best2, lists:seq(1, Cols)),
    Best4 = lists:foldl(fun(Col, AccBest) -> TraverseFun({Rows,Col}, bottom, AccBest) end, Best3, lists:seq(1, Cols)),
    Best4.
    
    
traverse_and_count(Mirrors, MapSize, Start, Direction) ->
    Traversed = traverse_map(Mirrors, MapSize, Start, Direction, []),
    count_traversed(Traversed).

traverse_map(_Mirrors, _MapSize, {0, _}, _Direction, AccTraversed)               -> AccTraversed;
traverse_map(_Mirrors, _MapSize, {_, 0}, _Direction, AccTraversed)               -> AccTraversed;
traverse_map(_Mirrors, {Rows,_}, {R, _}, _Direction, AccTraversed) when R > Rows -> AccTraversed;
traverse_map(_Mirrors, {_,Cols}, {_, C}, _Direction, AccTraversed) when C > Cols -> AccTraversed;
traverse_map(Mirrors,  MapSize,  Index,  Direction,  AccTraversed) ->
    {IsTraversed, NewAccTraversed} = case lists:keyfind(Index, 1, AccTraversed) of
        false ->
            {false, [{Index,[Direction]}|AccTraversed]};
        {Index, Directions} ->
            case lists:member(Direction, Directions) of
                true  -> {true, AccTraversed};
                false -> {false, lists:keyreplace(Index, 1, AccTraversed, {Index, [Direction|Directions]})}
            end
    end,
    case IsTraversed of
        true ->
            NewAccTraversed;
        false ->
            {Row, Column} = Index,
            case {find_mirror(Index, Mirrors), Direction} of
                {false,      left  } ->       traverse_map(Mirrors, MapSize, {Row, Column+1}, left,   NewAccTraversed);
                {false,      top   } ->       traverse_map(Mirrors, MapSize, {Row+1, Column}, top,    NewAccTraversed);
                {false,      right } ->       traverse_map(Mirrors, MapSize, {Row, Column-1}, right,  NewAccTraversed);
                {false,      bottom} ->       traverse_map(Mirrors, MapSize, {Row-1, Column}, bottom, NewAccTraversed);
                {f_slash,    left  } ->       traverse_map(Mirrors, MapSize, {Row-1, Column}, bottom, NewAccTraversed);
                {f_slash,    top   } ->       traverse_map(Mirrors, MapSize, {Row, Column-1}, right,  NewAccTraversed);
                {f_slash,    right } ->       traverse_map(Mirrors, MapSize, {Row+1, Column}, top,    NewAccTraversed);
                {f_slash,    bottom} ->       traverse_map(Mirrors, MapSize, {Row, Column+1}, left,   NewAccTraversed);
                {b_slash,    left  } ->       traverse_map(Mirrors, MapSize, {Row+1, Column}, top,    NewAccTraversed);
                {b_slash,    top   } ->       traverse_map(Mirrors, MapSize, {Row, Column+1}, left,   NewAccTraversed);
                {b_slash,    right } ->       traverse_map(Mirrors, MapSize, {Row-1, Column}, bottom, NewAccTraversed);
                {b_slash,    bottom} ->       traverse_map(Mirrors, MapSize, {Row, Column-1}, right,  NewAccTraversed);
                {top_down,   top   } ->       traverse_map(Mirrors, MapSize, {Row+1, Column}, top,    NewAccTraversed);
                {top_down,   bottom} ->       traverse_map(Mirrors, MapSize, {Row-1, Column}, bottom, NewAccTraversed);
                {top_down,   _Other} -> NAT = traverse_map(Mirrors, MapSize, {Row-1, Column}, bottom, NewAccTraversed),
                                              traverse_map(Mirrors, MapSize, {Row+1, Column}, top,    NAT            );
                {left_right, left  } ->       traverse_map(Mirrors, MapSize, {Row, Column+1}, left,   NewAccTraversed);
                {left_right, right } ->       traverse_map(Mirrors, MapSize, {Row, Column-1}, right,  NewAccTraversed);
                {left_right, _Other} -> NAT = traverse_map(Mirrors, MapSize, {Row, Column-1}, right,  NewAccTraversed),
                                              traverse_map(Mirrors, MapSize, {Row, Column+1}, left,   NAT            )
           end
   end.
   
   
find_mirror({Row, Column}, Mirrors) ->
    case lists:keyfind(Row, 1, Mirrors) of
        false ->
            false;
        {Row, RowMirrors} ->
            case lists:keyfind(Column, 1, RowMirrors) of
                false            -> false;
                {Column, Mirror} -> Mirror
            end
    end.    
            
    
count_traversed(Traversed) ->
    erlang:length(Traversed).    


trim_ending_newline(Str) ->
    string:sub_string(Str, 1, string:len(Str) - 1).

