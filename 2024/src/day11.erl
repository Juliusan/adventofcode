-module(day11).
-export([solve/2]).

% Labai jau paprastai gavosi. Gerokai trumpiau dirbau, nei kelias pastarasias
% dienas. Tiesa, po pirmos dalies programą reikėjo gerokai patobulinti.

% (aoc_2024@JuliusErisataT14.erisata.lt)1> day11:solve("priv/day11-PVZ1.txt", 1).
% 7
% (aoc_2024@JuliusErisataT14.erisata.lt)2> day11:solve("priv/day11-PVZ2.txt", 6).
% 22
% (aoc_2024@JuliusErisataT14.erisata.lt)3> day11:solve("priv/day11-PVZ2.txt", 25).
% 55312
% (aoc_2024@JuliusErisataT14.erisata.lt)4> day11:solve("priv/day11.txt", 25).
% 217443
% (aoc_2024@JuliusErisataT14.erisata.lt)5> day11:solve("priv/day11.txt", 75).
% 257246536026785
% (aoc_2024@JuliusErisataT14.erisata.lt)6> timer:tc(fun() -> day11:solve("priv/day11.txt", 25) end).
% {5031,217443}
% (aoc_2024@JuliusErisataT14.erisata.lt)7> timer:tc(fun() -> day11:solve("priv/day11.txt", 75) end).
% {253091,257246536026785}


read_stones(FileName) ->
    Line = utils:read_only_line_no_new_line(FileName),
    utils:get_integer_list(Line).


blink_stone(0) -> [1];
blink_stone(Stone) ->
    Digits = utils:integer_digit_count(Stone),
    case Digits rem 2 of
        0 -> erlang:tuple_to_list(utils:split_integer(Stone, Digits div 2));
        _ -> [Stone*2024]
    end.


blinks_stones(Stones, 0, Acc) -> {erlang:length(Stones), Acc};
blinks_stones(Stones, Blinks, Acc) ->
    lists:foldl(fun(Stone, {AccCount, AccCountMap}) ->
        case maps:get({Stone, Blinks}, AccCountMap, undefined) of
            undefined ->
                NewStones = blink_stone(Stone),
                {Count, NewAccCountMap} = blinks_stones(NewStones, Blinks-1, AccCountMap),
                {AccCount+Count, NewAccCountMap#{{Stone, Blinks} => Count}};
            Count ->
                {AccCount+Count, AccCountMap}
        end
    end, {0, Acc}, Stones).


solve(FileName, Blinks) ->
    Stones = read_stones(FileName),
    %utils:print("~p", [Stones]),
    {Count, _} = blinks_stones(Stones, Blinks, #{}),
    Count.
