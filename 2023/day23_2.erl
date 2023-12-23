-module(day23_2).
-export([solve/1]).

% Padariau primityviai ir buvo per ilgai. Sugalvojau optimizaciją. Praėjo laiko kol ją užrašiau.
% Sukasi ~12 sekundžių, bet atsąkymą grąžina. Pasigūglinau, kad kažkokio efektyvaus algoritmo
% šitai problemai nėra.

solve(FileName) ->
    Map = get_map(FileName),
    {MapE, Size} = encode_map(Map),
    %io:fwrite("XXX ~p~n", [Size]),
    Graph = map_to_graph(MapE, Size),
    %io:fwrite("XXX ~p~n", [Graph]),	
    Result = traverse_graph(Graph, Size),
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
                {Column+1, AccME#{Column => Tile}}
        end, {1, #{}}, MapLine),
        {Row+1, AccMapE#{Row => NewMapE}}
    end, {1, #{}}, Map),
    [First|_] = Map,
    Rows = Rows1-1,
    Cols = erlang:length(First),
    {MapE, {Rows, Cols}}.
    
    
map_to_graph(Map, MapSize) ->
    {NextNode, Length} = path_to_node(Map, MapSize, {1,2}, {2,2}, 1),
    map_to_graph(Map, MapSize, [NextNode], #{{1,2} => [{NextNode, Length}]}).

map_to_graph(_Map, _MapSize, [], AccGraph) ->
    AccGraph;
    
map_to_graph(Map, MapSize, [Node|Nodes], AccGraph) ->
    Nexts = get_next_cells(Map, MapSize, Node),
    {NextNodes, NewNodes} = lists:foldl(fun(Next, {AccEdges, AccN}) ->
        {NextNode, Length} = path_to_node(Map, MapSize, Node, Next, 1),
        case NextNode of
            Node -> AccGraph;
            _ ->
                NewAccEdges = [{NextNode, Length}|AccEdges],
                NewAccN = case maps:find(NextNode, AccGraph) of
                    {ok, _} -> AccN;
                    error   -> [NextNode|AccN]
                end,
                {NewAccEdges, NewAccN}
        end
    end, {[], Nodes}, Nexts),
    NewAccMap = AccGraph#{Node => NextNodes},
    map_to_graph(Map, MapSize, NewNodes, NewAccMap).

    
get_next_cells(Map, {Rows, Cols}, {Row, Col}) ->
    Right = case Col =:= Cols of
        true  -> [];
        false -> [{Row, Col+1}]
    end,
    Down = case Row =:= Rows of
        true  -> [];
        false -> [{Row+1, Col}]
    end,
    Left = case Col of
        1 -> [];
        _ -> [{Row, Col-1}]
    end,
    Up = case Row of
        1 -> [];
        _ -> [{Row-1, Col}]
    end,
    AllCases = Right ++ Down ++ Left ++ Up,
    lists:filter(fun(Next) ->
        case get_cell(Next, Map) of
            $# -> false;
            $. -> true;
            $> -> true;
            $v -> true;
            $< -> true;
            $^ -> true
        end
    end, AllCases).


path_to_node(Map, MapSize, From, To, Length) ->
    AllNextCells = get_next_cells(Map, MapSize, To),
    NextCells = lists:delete(From, AllNextCells),
    case NextCells of
        []      -> {To, Length};
        [Next]  -> path_to_node(Map, MapSize, To, Next, Length+1);
        [_,_|_] -> {To, Length}
    end.


traverse_graph(Graph, {Rows, Cols}) ->
    Length = traverse_graph(Graph, {Rows, Cols-1}, {1,2}, 0, []),
    Length.

    
traverse_graph(_Graph, {RowT, ColT}, {RowT, ColT}, Length, _) ->
    %io:fwrite("XXX EUREKA! ~p~n", [Length]),
    Length;
    
traverse_graph(Graph, Target, Node, Length, Visited) ->
    %io:fwrite("XXX TRAVERSE ~p ~p ~p~n", [Node, Length, Visited]),
    #{Node := NextNodes} = Graph,
    lists:foldl(fun({NextNode, StepLength}, AccLength) ->
        case lists:member(NextNode, Visited) of
            true ->
                %io:fwrite("    XXX ~p ~p ~p -> FALSE~n", [Node, Length, Visited]),
                AccLength;
            false ->
                NextLength = traverse_graph(Graph, Target, NextNode, Length+StepLength, [Node|Visited]),
                Result = case {NextLength, AccLength} of
                    {_, false}        -> NextLength;
                    {false, _}        -> AccLength;
                    {N, A} when N > A -> NextLength;
                    _                 -> AccLength
                end,
                %io:fwrite("    XXX ~p ~p ~p -> ~p ~p ~p~n", [Node, Length, Visited, NextLength, AccLength, Result]),
                Result
        end
    end, false, NextNodes).
    
    
get_cell({Row, Col}, Map) ->
    #{Row := RowMap} = Map,
    #{Col := Tile} = RowMap,
    Tile.


trim_ending_newline(Str) ->
    string:sub_string(Str, 1, string:len(Str) - 1).

