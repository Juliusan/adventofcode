-module(day01_2).
-export([solve/1]).

% Užstrigau su "eightwo" gale
%
% 69> c(day01_2).                   
% {ok,day01_2}
% 70> day01_2:solve("day01-IN.txt").
% 55429

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
    %io:fwrite("XXX ~p - ", [Line]),
    {First, Last} = get_number(Line, undefined, undefined),
    Result = First * 10 + Last,
    %io:fwrite("XXX ~p~n", [Result]),
    Result.
    
get_number([10], First, Last) ->
    {First, Last};

get_number([Char | Line], First, _Last) when 48 =< Char, Char =< 57 ->
    Digit =  Char - 48,
    NewFirst = get_new_first_char(First, Digit),
    get_number(Line, NewFirst, Digit);

get_number("one" ++ Line, First, _Last) ->
    NewFirst = get_new_first_char(First, 1),
    get_number("ne" ++ Line, NewFirst, 1);

get_number("two" ++ Line, First, _Last) ->
    NewFirst = get_new_first_char(First, 2),
    get_number("wo" ++ Line, NewFirst, 2);

get_number("three" ++ Line, First, _Last) ->
    NewFirst = get_new_first_char(First, 3),
    get_number("hree" ++ Line, NewFirst, 3);

get_number("four" ++ Line, First, _Last) ->
    NewFirst = get_new_first_char(First, 4),
    get_number("our" ++ Line, NewFirst, 4);

get_number("five" ++ Line, First, _Last) ->
    NewFirst = get_new_first_char(First, 5),
    get_number("ive" ++ Line, NewFirst, 5);

get_number("six" ++ Line, First, _Last) ->
    NewFirst = get_new_first_char(First, 6),
    get_number("ix" ++ Line, NewFirst, 6);

get_number("seven" ++ Line, First, _Last) ->
    NewFirst = get_new_first_char(First, 7),
    get_number("even" ++ Line, NewFirst, 7);

get_number("eight" ++ Line, First, _Last) ->
    NewFirst = get_new_first_char(First, 8),
    get_number("ight" ++ Line, NewFirst, 8);

get_number("nine" ++ Line, First, _Last) ->
    NewFirst = get_new_first_char(First, 9),
    get_number("ine" ++ Line, NewFirst, 9);
    
get_number([_ | Line], First, Last) ->
    get_number(Line, First, Last).
    
    
get_new_first_char(undefined, NewChar) -> NewChar;
get_new_first_char(OldChar,  _NewChar) -> OldChar.
