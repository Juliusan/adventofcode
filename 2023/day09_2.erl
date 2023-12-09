-module(day09_2).
-export([solve/1]).

% 00:02:42 - Čia net per lengva kaip antrai daliai. Didžiąją laiko dalį užėmė kopijavimas. Pakeisti tereikėjo vieną eilutę.
%            Pasirodo, teisingas atsakymas gaunasi ir tiesiog nuėmus "reverse" iš pirmos dalies get_values/1 funkcijos. Čia įdomi matematinė užduotis tą įrodyti :-D

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
    numbers_str_to_list(LineNoNewLine).


get_zeros(Values) ->
    [Last | _] = Values,
    get_zeros(Values, [Last]).
        

get_zeros(Values, Lasts) ->
    %io:fwrite("XXX ~p ~p ~n", [Values, Lasts]),
    case lists:all(fun(Value) -> Value =:= 0 end, Values) of
        true ->
            [0 | OtherLasts] = Lasts,
            FinalPrev = lists:foldl(fun(Last, Prev) ->
                %io:fwrite("XXX-> ~p ~p -> ~p~n", [Last, Prev, Last - Prev]),
                Last - Prev
            end, 0, OtherLasts),
            %io:fwrite("XXX SUM-> ~p~n", [FinalPrev]),
            FinalPrev;
        false ->
            NewValues = get_diffs(Values),
            [Last | _] = NewValues,
            get_zeros(NewValues, [Last | Lasts])
    end.


get_diffs([_          ]) -> [];
get_diffs([A, B | Rest]) -> [B - A | get_diffs([B | Rest])].
    

trim_ending_newline(Str) ->
    string:sub_string(Str, 1, string:len(Str) - 1).
        
    
numbers_str_to_list(NumbersStr) ->
    NumbersStrList = string:split(NumbersStr, " ", all),
    lists:filtermap(fun
        ("")        -> false;
        (NumberStr) -> {true, erlang:list_to_integer(NumberStr)}
    end, NumbersStrList).

