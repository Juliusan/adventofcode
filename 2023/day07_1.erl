-module(day07_1).
-export([solve/1]).

% 00:40:31 - Nesudėtinga. Tik programavimas.

solve(FileName) ->
    {ok, File} = file:open(FileName, [read]),
    GetHandsFun = fun GetHandsFun(AccHands) ->
        case file:read_line(File) of
            eof ->
                AccHands;
            {ok, Line} ->
                %io:fwrite("XXX ~p: ", [Line]),
                HandBid = get_hand(Line),
                %io:fwrite("~p~n", [HandBid]),
                GetHandsFun([HandBid | AccHands])
        end
    end,
    Hands = GetHandsFun([]),
    ok = file:close(File),
    HandsSorted = sort_hands(Hands),
    HandsRanked = rank_hands(HandsSorted),
    Winnings = get_winnings(HandsRanked),
    Winnings.


get_hand(Line) -> 
    LineNoNewLine = trim_ending_newline(Line), 
    [CardsStr, BidStr] = string:split(LineNoNewLine, " "),
    [C1Str, C2Str, C3Str, C4Str, C5Str] = CardsStr,
    Card1 = card_str_to_card(C1Str),
    Card2 = card_str_to_card(C2Str),
    Card3 = card_str_to_card(C3Str),
    Card4 = card_str_to_card(C4Str),
    Card5 = card_str_to_card(C5Str),
    Cards = [Card1, Card2, Card3, Card4, Card5],
    Bid = erlang:list_to_integer(BidStr),
    {Cards, Bid}.
    
    
sort_hands(Hands) ->
    lists:sort(fun({Cards1, _Bid1}, {Cards2, _Bid2}) ->
        Type1 = get_hand_type(Cards1),
        Type2 = get_hand_type(Cards2),
        case {Type1, Type2} of
            {T, T} -> Cards1 < Cards2;
            _      -> Type1 < Type2
        end
    end, Hands).
    
    
get_hand_type(Cards) ->
    CardsCount = lists:foldl(fun(Card, AccCardsCount) ->
        CardCount = erlang:element(Card, AccCardsCount),
        erlang:setelement(Card, AccCardsCount, CardCount + 1)
    end, {0,0,0,0,0,0,0,0,0,0,0,0,0}, Cards),
    CardsCountNoZeros = lists:filter(fun(D) -> D =/= 0 end, erlang:tuple_to_list(CardsCount)),
    CardsCountSorted = lists:reverse(lists:sort(CardsCountNoZeros)),
    case CardsCountSorted of
        [5]             -> 7;
        [4 | _]         -> 6;
        [3, 2 ]         -> 5;
        [3 | _]         -> 4;
        [2, 2 | _]      -> 3;
        [2 | _]         -> 2;
        [1, 1, 1, 1, 1] -> 1
    end.
    

rank_hands(Hands) ->
    lists:zip(Hands, lists:seq(1, erlang:length(Hands))).
    
    
get_winnings(Hands) ->
    lists:foldl(fun({{_Hand, Bid}, Rank}, AccWinnings) ->
        AccWinnings + Bid*Rank
    end, 0, Hands).
    
    
card_str_to_card($A) -> 13; 
card_str_to_card($K) -> 12;
card_str_to_card($Q) -> 11;
card_str_to_card($J) -> 10;
card_str_to_card($T) ->  9;
card_str_to_card(D) when $2 =< D, D =< $9 -> D-$1.

    
trim_ending_newline(Str) ->
    string:sub_string(Str, 1, string:len(Str) - 1).

