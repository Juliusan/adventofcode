-module(day03_2).
-export([solve/1]).

% Irgi tik sprendimas. Padariau vieną kvailą klaidą, bet didelių strigimų nebuvo.

solve(FileName) ->
    {ok, File} = file:open(FileName, [read]),
    ResultFun = fun ResultFun(AccNumbers, AccSymbols) ->
        case file:read_line(File) of
            eof ->
                {AccNumbers, AccSymbols};
            {ok, Line} ->
                %io:fwrite("XXX ~p: ", [Line]),
                {Numbers, Symbols} = get_numbers_and_symbols(Line),
                %io:fwrite("~p ~p~n", [Numbers, Symbols]),
                ResultFun([Numbers | AccNumbers], [Symbols | AccSymbols])
        end
    end,
    {FinalNumbers, FinalSymbols} = ResultFun([], []),
    ok = file:close(File),
    %io:fwrite("XXX ~p ~p~n", [FinalNumbers, FinalSymbols]),
    ValidNumbers = get_gears(FinalNumbers, FinalSymbols),
    %io:fwrite("XXX ~p~n", [ValidNumbers]),
    sum_numbers(ValidNumbers).


get_numbers_and_symbols(Line) -> 
    LineNoNewLine = string:sub_string(Line, 1, string:len(Line) - 1), 
    get_numbers_and_symbols(LineNoNewLine, 0, undefined, [], []).

get_numbers_and_symbols([], Index, AccNumber, AccNumbers, AccSymbols) ->
    NewAccNumbers = get_new_acc_numbers(AccNumber, Index, AccNumbers),
    {NewAccNumbers, AccSymbols};
    
get_numbers_and_symbols([DigitStr | Line], Index, AccNumber, AccNumbers, AccSymbols) when 48 =< DigitStr, DigitStr =< 57 ->
    Digit = DigitStr - 48,
    Number = case AccNumber of
        undefined -> {Index, Digit};
        {Start, AccN} -> {Start, AccN * 10 + Digit}
    end,
    get_numbers_and_symbols(Line, Index+1, Number, AccNumbers, AccSymbols);
    
get_numbers_and_symbols([$. | Line], Index, AccNumber, AccNumbers, AccSymbols) ->
    NewAccNumbers = get_new_acc_numbers(AccNumber, Index, AccNumbers),
    get_numbers_and_symbols(Line, Index+1, undefined, NewAccNumbers, AccSymbols);
    
get_numbers_and_symbols([_ | Line], Index, AccNumber, AccNumbers, AccSymbols) ->
    NewAccNumbers = get_new_acc_numbers(AccNumber, Index, AccNumbers),
    get_numbers_and_symbols(Line, Index+1, undefined, NewAccNumbers, [Index | AccSymbols]).
    
    
get_new_acc_numbers(undefined,       _,     AccNumbers) -> AccNumbers;
get_new_acc_numbers({Start, Number}, Index, AccNumbers) -> [{Start, Index-1, Number} | AccNumbers].


get_gears(Numbers, Symbols) ->
    NewNumbers = [[] | Numbers] ++ [[]],
    get_gears(NewNumbers, Symbols, []).

get_gears([_, []], [], AccGears) -> 
    AccGears;

get_gears([Numbers1, Numbers2, Numbers3 | RemNumbers], [Symbols| RemSymbols], AccGears) -> 
    AllNumbers = lists:append([Numbers1, Numbers2, Numbers3]),
    NewGears = lists:filtermap(fun(Index) ->
        Gears = lists:filter(fun({Start, End, _Number}) ->
            (Start =< Index-1 andalso Index-1 =< End) orelse
            (Start =< Index   andalso Index   =< End) orelse
            (Start =< Index+1 andalso Index+1 =< End)
        end, AllNumbers),
        %io:fwrite("~p: ~p~n", [Index, Gears]),
        case Gears of
            [{_, _, Number1}, {_, _, Number2}] -> {true, Number1 * Number2};
            _                                  -> false
        end
    end, Symbols),
    get_gears([Numbers2, Numbers3 | RemNumbers], RemSymbols, NewGears ++ AccGears).
        

sum_numbers(Numbers) -> lists:sum(Numbers).

