-module(day04_2).
-export([solve/1]).

% 0:21:18 - kiek sunkesnė, nei pirma dalis, bet gavosi be didelių problemų.

solve(FileName) ->
    {ok, File} = file:open(FileName, [read]),
    ResultFun = fun ResultFun(Sum, CardsWon) ->
        case file:read_line(File) of
            eof ->
                {Sum, CardsWon};
            {ok, Line} ->
                {ID, Wins} = get_wins(Line),
                {CopiesWon, RemCardsWon} = get_copies_won(ID, CardsWon),
                Copies = CopiesWon + 1,
                NewCardsWon = add_cards_won(lists:seq(ID+1, ID+Wins), Copies, RemCardsWon),
                %io:fwrite("XXX ~p: ~p ", [ID, Games]),
                ResultFun(Sum + Copies, NewCardsWon)
        end
    end,
    {Result, []} = ResultFun(0, []),
    ok = file:close(File),
    Result.
    

get_wins("Card " ++ Rest) ->
    [IDStr, NumbersStr] = string:split(Rest, ": "),
    ID = erlang:list_to_integer(string:trim(IDStr)),
    NumbersStrNoNewLine = string:sub_string(NumbersStr, 1, string:len(NumbersStr) - 1), 
    %io:fwrite("XXX ~p: ~p~n", [ID, GamesStr]),
    [WNumbersStr, MyNumbersStr] = string:split(NumbersStrNoNewLine, " | "),
    WNumbers  = numbers_str_to_list(WNumbersStr),
    MyNumbers = numbers_str_to_list(MyNumbersStr),
    %io:fwrite("XXX ~p: ~p~n", [ID, GamesListStr]),
    GoodNumbers = lists:filter(fun(Number) -> lists:member(Number, WNumbers) end, MyNumbers),
    {ID, erlang:length(GoodNumbers)}.
    
    
numbers_str_to_list(NumbersStr) ->
    NumbersStrList = string:split(NumbersStr, " ", all),
    lists:filtermap(fun
        ("")        -> false;
        (NumberStr) -> {true, erlang:list_to_integer(NumberStr)}
    end, NumbersStrList).
    

get_copies_won(ID, CardsWon) ->
    case lists:keytake(ID, 1, CardsWon) of
        false                              -> {0,      CardsWon};
        {value, {ID, Copies}, RemCardsWon} -> {Copies, RemCardsWon}
    end.
    
    
add_cards_won(Cards, Copies, CardsWon) ->
    lists:foldl(fun(Card, AccCardsWon) ->
        case lists:keytake(Card, 1, AccCardsWon) of
            false                              -> [ {Card, Copies}   | AccCardsWon ];
            {value, {Card, C}, RemAccCardsWon} -> [ {Card, C+Copies} | RemAccCardsWon]
        end
    end, CardsWon, Cards).

