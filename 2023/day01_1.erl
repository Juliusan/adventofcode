-module(day01_1).
-export([solve/1]).

% Daugiausia kovojau su Erlango skaitymu iš failo ir išvedimu į ekraną.
%
% 38> c(day01_1).                     
% {ok,day01_1}
% 39> day01_1:solve("day01-IN.txt").
% 54605

solve(FileName) ->
    {ok, File} = file:open(FileName, [read]),
    ResultFun = fun ResultFun(Sum) ->
        case file:read_line(File) of
            eof ->
                Sum;
            {ok, Line} ->
                NewSum = Sum + get_number(Line),
                ResultFun(NewSum)
        end
    end,
    Result = ResultFun(0),
    ok = file:close(File),
    Result.
    
    
get_number(Line) ->
    {First, Last} = get_number(Line, undefined, undefined),
    Result = First * 10 + Last,
    %io:fwrite("XXX ~p~n", [Result]),
    Result.
    
get_number([10], First, Last) ->
    {First-48, Last-48};

get_number([Char | Line], First, _Last) when 48 =< Char, Char =< 57 ->
    NewFirst = case First of
        undefined -> Char;
        _         -> First
    end,
    get_number(Line, NewFirst, Char);
    
get_number([_ | Line], First, Last) ->
    get_number(Line, First, Last).
