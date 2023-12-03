-module(day03_1).
-export([solve/1]).

% 00:39:44 - Kreivas uždavinys, bet išsprendžiau be didelių problemų

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
    ValidNumbers = get_valid_numbers(FinalNumbers, FinalSymbols),
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


get_valid_numbers(Numbers, Symbols) ->
    NewSymbols = [[] | Symbols] ++ [[]],
    get_valid_numbers(Numbers, NewSymbols, []).

get_valid_numbers([], [_, []], AccValidNumbers) -> 
    AccValidNumbers;

get_valid_numbers([Numbers | RemNumbers], [Symbols1, Symbols2, Symbols3 | RemSymbols], AccValidNumbers) -> 
    AllSymbols = lists:append([Symbols1, Symbols2, Symbols3]),
    NewValidNumbers = lists:filtermap(fun({Start, End, Number}) ->
        HasAdjSymbol = lists:any(fun(Index) ->
            lists:member(Index, AllSymbols)
        end, lists:seq(Start-1, End+1)),
        case HasAdjSymbol of
            true  -> {true, Number};
            false -> false
        end
    end, Numbers),
    get_valid_numbers(RemNumbers, [Symbols2, Symbols3 | RemSymbols], NewValidNumbers ++ AccValidNumbers).
        

sum_numbers(Numbers) -> lists:sum(Numbers).

