-module(day02_1).
-export([solve/2]).

% Nieko mandro. Užtruko kodinimas.
%
% 53> c(day02_1).                                    
% {ok,day02_1}
% 54> day02_1:solve("day02-IN.txt", {12, 13, 14}).
% 2061

solve(FileName, Config) ->
    {ok, File} = file:open(FileName, [read]),
    ResultFun = fun ResultFun(Sum) ->
        case file:read_line(File) of
            eof ->
                Sum;
            {ok, Line} ->
                {ID, Games} = get_games(Line),
                %io:fwrite("XXX ~p: ~p ", [ID, Games]),
                case is_game_possible(Games, Config) of
                    true  -> ResultFun(Sum + ID);
                    false -> ResultFun(Sum)
                end
        end
    end,
    Result = ResultFun(0),
    ok = file:close(File),
    Result.
    

get_games("Game " ++ Rest) ->
    [IDStr, GamesStrNewLine] = string:split(Rest, ": "),
    ID = erlang:list_to_integer(IDStr),
    GamesStr = string:sub_string(GamesStrNewLine, 1, string:len(GamesStrNewLine) - 1), 
    %io:fwrite("XXX ~p: ~p~n", [ID, GamesStr]),
    GamesListStr = string:split(GamesStr, "; ", all),
    %io:fwrite("XXX ~p: ~p~n", [ID, GamesListStr]),
    Games = lists:map(fun(GameStr) ->
        CubesStr = string:split(GameStr, ", ", all),
        lists:foldl(fun(CubeStr, {Red, Green, Blue}) ->
            [NumberStr, Colour] = string:split(CubeStr, " "),
            Number = erlang:list_to_integer(NumberStr),
            case {Colour, Red, Green, Blue} of
                {"red",   0, _, _} -> {Number, Green,  Blue  };
                {"green", _, 0, _} -> {Red,    Number, Blue  };
                {"blue",  _, _, 0} -> {Red,    Green,  Number}
            end
        end, {0, 0, 0}, CubesStr)
    end, GamesListStr),
    %io:fwrite("XXX ~p: ~p~n", [ID, Games]),
    {ID, Games}.
    
    
is_game_possible(Games, {RedC, GreenC, BlueC}) -> 
    lists:all(fun({Red, Green, Blue}) ->
        Red =< RedC andalso Green =< GreenC andalso Blue =< BlueC
    end, Games).

