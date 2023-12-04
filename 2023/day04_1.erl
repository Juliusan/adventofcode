-module(day04_1).
-export([solve/1]).

% ~19 minučių - labai lengva.

solve(FileName) ->
    {ok, File} = file:open(FileName, [read]),
    ResultFun = fun ResultFun(Sum) ->
        case file:read_line(File) of
            eof ->
                Sum;
            {ok, Line} ->
                Points = get_points(Line),
                ResultFun(Points + Sum)
        end
    end,
    Result = ResultFun(0),
    ok = file:close(File),
    Result.
    

get_points("Card " ++ Rest) ->
    [_IDStr, NumbersStr] = string:split(Rest, ": "),
    %ID = erlang:list_to_integer(string:trim(IDStr)),
    NumbersStrNoNewLine = string:sub_string(NumbersStr, 1, string:len(NumbersStr) - 1), 
    [WNumbersStr, MyNumbersStr] = string:split(NumbersStrNoNewLine, " | "),
    WNumbers  = numbers_str_to_list(WNumbersStr),
    MyNumbers = numbers_str_to_list(MyNumbersStr),
    GoodNumbers = lists:filter(fun(Number) -> lists:member(Number, WNumbers) end, MyNumbers),
    erlang:trunc(math:pow(2, erlang:length(GoodNumbers) - 1)).
    
    
numbers_str_to_list(NumbersStr) ->
    NumbersStrList = string:split(NumbersStr, " ", all),
    lists:filtermap(fun
        ("")        -> false;
        (NumberStr) -> {true, erlang:list_to_integer(NumberStr)}
    end, NumbersStrList).
