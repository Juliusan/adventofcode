-module(day16_1).
-export([solve/1]).

% Nesudėtinga. Tik programavimas. Kažkiek užtruko dideliame žemėlapyje. Žiūrėsime, kas bus toliau.

solve(FileName) ->
    Map = get_map(FileName),
    {Mirrors, Size} = get_mirrors(Map),
    Traversed = traverse_map(Mirrors, Size),
    Result = count_traversed(Traversed),
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
                NewM = {{Row, Column}, MirrorAtom},
                {Column+1, [NewM | AccMs]}
        end, {1, AccMirrors}, MapLine),
        {Row+1, NewMirrors}
    end, {1, []}, Map),
    [First|_] = Map,
    Rows = Rows1-1,
    Cols = erlang:length(First),
    {Mirrors, {Rows, Cols}}.
    
    
traverse_map(Mirrors, MapSize) ->
    traverse_map(Mirrors, MapSize, {1,1}, left, []).

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
            case {lists:keyfind(Index, 1, Mirrors), Direction} of
                {false,               left  } ->       traverse_map(Mirrors, MapSize, {Row, Column+1}, left,   NewAccTraversed);
                {false,               top   } ->       traverse_map(Mirrors, MapSize, {Row+1, Column}, top,    NewAccTraversed);
                {false,               right } ->       traverse_map(Mirrors, MapSize, {Row, Column-1}, right,  NewAccTraversed);
                {false,               bottom} ->       traverse_map(Mirrors, MapSize, {Row-1, Column}, bottom, NewAccTraversed);
                {{Index, f_slash},    left  } ->       traverse_map(Mirrors, MapSize, {Row-1, Column}, bottom, NewAccTraversed);
                {{Index, f_slash},    top   } ->       traverse_map(Mirrors, MapSize, {Row, Column-1}, right,  NewAccTraversed);
                {{Index, f_slash},    right } ->       traverse_map(Mirrors, MapSize, {Row+1, Column}, top,    NewAccTraversed);
                {{Index, f_slash},    bottom} ->       traverse_map(Mirrors, MapSize, {Row, Column+1}, left,   NewAccTraversed);
                {{Index, b_slash},    left  } ->       traverse_map(Mirrors, MapSize, {Row+1, Column}, top,    NewAccTraversed);
                {{Index, b_slash},    top   } ->       traverse_map(Mirrors, MapSize, {Row, Column+1}, left,   NewAccTraversed);
                {{Index, b_slash},    right } ->       traverse_map(Mirrors, MapSize, {Row-1, Column}, bottom, NewAccTraversed);
                {{Index, b_slash},    bottom} ->       traverse_map(Mirrors, MapSize, {Row, Column-1}, right,  NewAccTraversed);
                {{Index, top_down},   top   } ->       traverse_map(Mirrors, MapSize, {Row+1, Column}, top,    NewAccTraversed);
                {{Index, top_down},   bottom} ->       traverse_map(Mirrors, MapSize, {Row-1, Column}, bottom, NewAccTraversed);
                {{Index, top_down},   _Other} -> NAT = traverse_map(Mirrors, MapSize, {Row-1, Column}, bottom, NewAccTraversed),
                                                       traverse_map(Mirrors, MapSize, {Row+1, Column}, top,    NAT            );
                {{Index, left_right}, left  } ->       traverse_map(Mirrors, MapSize, {Row, Column+1}, left,   NewAccTraversed);
                {{Index, left_right}, right } ->       traverse_map(Mirrors, MapSize, {Row, Column-1}, right,  NewAccTraversed);
                {{Index, left_right}, _Other} -> NAT = traverse_map(Mirrors, MapSize, {Row, Column-1}, right,  NewAccTraversed),
                                                       traverse_map(Mirrors, MapSize, {Row, Column+1}, left,   NAT            )
           end
   end.
            
    
count_traversed(Traversed) ->
    erlang:length(Traversed).    


trim_ending_newline(Str) ->
    string:sub_string(Str, 1, string:len(Str) - 1).

