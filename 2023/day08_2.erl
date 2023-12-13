-module(day08_2).
-export([solve/1]).

% Nesudėtinga. Tik antrą kartą užsiroviau ant uždavinio dydžio. Bandžiau daryti kaip užduotyje: visus kelius privesti iki galo bendrai.
% Turėjau tokį kodą (užkomentuotas). Pasirodo, net erlangui ne taip lengva iki 17 tūkstančių milijardų suskaičiuoti. Taigi, užtruko perrašyti.
% Ir, pasirodo, mano algoritmas turi prielaidą (kurios sąlygoje nebuvo), kad jeigu yra kelias iš __A į __Z, tai kitas žingznis po __Z bus būtent viršūnė __A.

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
    %io:fwrite("XXX ~p~n: ", [Map]),
    StartNodes = get_start_nodes(Map),
    %io:fwrite("XXX ~p~n", [StartNodes]),
    Steps = count_steps(StartNodes, Path, Map),
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
    
    
get_start_nodes(Map) ->
    lists:filtermap(fun
        ({[_, _, $A] = Name, _, _}) -> {true, Name};
        ({_,                 _, _}) -> false
    end, Map).
    
    
count_steps(StartNodes, Path, Map) ->
    Steps = lists:map(fun(StartNode) ->
        %io:fwrite("XXX Start ~p~n", [StartNode]),
        count_steps(StartNode, Path, Map, 0, Path)
    end, StartNodes),
    %io:fwrite("XXX Start ~p~n", [Steps]),
    [First | Others] = Steps,
    lists:foldl(fun(Step, AccLCC) ->
        Step*AccLCC div gcd(Step, AccLCC)
    end, First, Others).


count_steps([_, _, $Z], _Path, _Map, Steps, _AllPath) ->
    Steps;
    
count_steps(Node, [], Map, Steps, AllPath) ->
    count_steps(Node, AllPath, Map, Steps, AllPath);
    
count_steps(Node, [Move | Path], Map, Steps, AllPath) ->
    {Node, Left, Right} = lists:keyfind(Node, 1, Map),
    NextNode = case Move of
        left  -> Left;
        right -> Right
    end,
    count_steps(NextNode, Path, Map, Steps+1, AllPath).


%count_steps(StartNodes, Path, Map) ->
%    count_steps(StartNodes, Path, Map, 0, Path).
%
%count_steps(Nodes, [], Map, Steps, AllPath) ->
%    count_steps(Nodes, AllPath, Map, Steps, AllPath);
%    
%count_steps(Nodes, [Move | Path], Map, Steps, AllPath) ->
%    IsEnd = lists:all(fun
%        ([_, _, $Z]) -> true;
%        (_         ) -> false
%    end, Nodes),
%    case IsEnd of
%        true ->
%            Steps;
%        false ->
%            %io:fwrite("XXX ~p~n: ", [Nodes]),
%            NextNodes = lists:map(fun(Node) ->
%                {Node, Left, Right} = lists:keyfind(Node, 1, Map),
%                case Move of
%                    left  -> Left;
%                    right -> Right
%                end
%            end, Nodes),
%            count_steps(NextNodes, Path, Map, Steps+1, AllPath)
%    end.
    
    
gcd(A, 0)            -> A;
gcd(A, B) when A < B -> gcd(B, A);
gcd(A, B)            -> gcd(B, A-B).

    

trim_ending_newline(Str) ->
    string:sub_string(Str, 1, string:len(Str) - 1).

