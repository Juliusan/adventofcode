-module(day02_2).
-export([solve/1]).

% 00:07:22 - Irgi nieko mandro. Paprastas pakeitimas.
%
%60> c(day02_2).                      
%{ok,day02_2}
%62> day02_2:solve("day02-IN.txt").   
%72596

solve(FileName) ->
    {ok, File} = file:open(FileName, [read]),
    ResultFun = fun ResultFun(Sum) ->
        case file:read_line(File) of
            eof ->
                Sum;
            {ok, Line} ->
                {_ID, Games} = get_games(Line),
                %io:fwrite("XXX ~p: ~p ", [ID, Games]),
                Power = game_power(Games),
                ResultFun(Sum + Power)
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
    
    
game_power(Games) ->
    {RedF, GreenF, BlueF} = lists:foldl(fun({Red, Green, Blue}, {RedM, GreenM, BlueM}) ->
        {lists:max([Red, RedM]), lists:max([Green, GreenM]), lists:max([Blue, BlueM])}
    end, {0, 0, 0}, Games),
    RedF * GreenF * BlueF.

