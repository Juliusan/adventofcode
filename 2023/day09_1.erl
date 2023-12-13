-module(day09_1).
-export([solve/1]).

% Nesudėtinga. Tik programavimas.

solve(FileName) ->
    {ok, File} = file:open(FileName, [read]),
    GetSumFun = fun GetSumFun(AccSum) ->
        case file:read_line(File) of
            eof ->
                AccSum;
            {ok, Line} ->
                %io:fwrite("XXX ~p: ", [Line]),
                Values = get_values(Line),
                NextElem = get_zeros(Values),
                %io:fwrite("~p~n", [MapNode]),
                GetSumFun(NextElem + AccSum)
        end
    end,
    Sum = GetSumFun(0),
    ok = file:close(File),
    Sum.
    
    
get_values(Line) ->
    LineNoNewLine = trim_ending_newline(Line),
    lists:reverse(numbers_str_to_list(LineNoNewLine)).


get_zeros(Values) ->
    [Last | _] = Values,
    get_zeros(Values, [Last]).
        

get_zeros(Values, Lasts) ->
    io:fwrite("XXX ~p ~p ~n", [Values, Lasts]),
    case lists:all(fun(Value) -> Value =:= 0 end, Values) of
        true ->
            lists:sum(Lasts);
        false ->
            NewValues = get_diffs(Values),
            [Last | _] = NewValues,
            get_zeros(NewValues, [Last | Lasts])
    end.


get_diffs([_          ]) -> [];
get_diffs([A, B | Rest]) -> [A - B | get_diffs([B | Rest])].
    

trim_ending_newline(Str) ->
    string:sub_string(Str, 1, string:len(Str) - 1).
        
    
numbers_str_to_list(NumbersStr) ->
    NumbersStrList = string:split(NumbersStr, " ", all),
    lists:filtermap(fun
        ("")        -> false;
        (NumberStr) -> {true, erlang:list_to_integer(NumberStr)}
    end, NumbersStrList).

