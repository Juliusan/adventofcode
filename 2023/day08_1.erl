-module(day08_1).
-export([solve/1]).

% 00:31:24 - Nesudėtinga. Tik programavimas. Neprižiūrėjau, kad pradėti visada reikia AAA, o baigti ZZZ, gavau (turbūt) amžiną ciklą.

solve(FileName) ->
    {ok, File} = file:open(FileName, [read]),
    Path = read_path(File),
    {ok, "\n"} = file:read_line(File),
    GetMapFun = fun GetMapFun(AccMap) ->
        case file:read_line(File) of
            eof ->
                AccMap;
            {ok, Line} ->
                %io:fwrite("XXX ~p: ", [Line]),
                MapNode = get_map_node(Line),
                %io:fwrite("~p~n", [MapNode]),
                GetMapFun([MapNode | AccMap])
        end
    end,
    Map = GetMapFun([]),
    ok = file:close(File),
    Steps = count_steps("AAA", "ZZZ", Path, Map),
    Steps.


read_path(File) ->
    {ok, PathStr} = file:read_line(File),
    PathStrNoNewLine = trim_ending_newline(PathStr),
    lists:map(fun
        ($L) -> left;
        ($R) -> right
    end, PathStrNoNewLine).
    

get_map_node([N1, N2, N3, 32, $=, 32, $(, L1, L2, L3, $,, 32, R1, R2, R3, $) | "\n"]) ->
    Node  = [N1, N2, N3],
    Left  = [L1, L2, L3],
    Right = [R1, R2, R3],
    {Node, Left, Right}.
    
    
count_steps(StartNode, EndNode, Path, Map) ->
    count_steps(StartNode, EndNode, Path, Map, 0, Path).


count_steps(EndNode, EndNode, _Path, _Map, Steps, _AllPath) ->
    Steps;
    
count_steps(Node, EndNode, [], Map, Steps, AllPath) ->
    count_steps(Node, EndNode, AllPath, Map, Steps, AllPath);
    
count_steps(Node, EndNode, [Move | Path], Map, Steps, AllPath) ->
    {Node, Left, Right} = lists:keyfind(Node, 1, Map),
    NextNode = case Move of
        left  -> Left;
        right -> Right
    end,
    count_steps(NextNode, EndNode, Path, Map, Steps+1, AllPath).
    

trim_ending_newline(Str) ->
    string:sub_string(Str, 1, string:len(Str) - 1).

