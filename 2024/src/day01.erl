-module(day01).
-export([solve_1/1, solve_2/1]).

% 21:57 11:49

% 46> c(utils).                        
% {ok,utils}
% 47> c(day01).                        
% {ok,day01}
% 48> day01:solve_1("day01-PVZ.txt").  
% 11
% 49> day01:solve_1("day01.txt").
% 2756096
% 50> day01:solve_2("day01-PVZ.txt").
% 31
% 51> day01:solve_2("day01.txt").    
% 23117829


read_line(Line) ->
    {Nr1, NewLine1} = utils:get_integer(Line),
    NewLine2 = string:trim(NewLine1, leading),
    {Nr2, _} = utils:get_integer(NewLine2),
    {Nr1, Nr2}.


read_sort_lists(FileName) ->
    Lists = utils:read_line_to_elem(FileName, fun read_line/1),
    {List1, List2} = lists:unzip(Lists),
    List1S = lists:sort(List1),
    List2S = lists:sort(List2),
    {List1S, List2S}.


solve_1(FileName) ->
    {List1S, List2S} = read_sort_lists(FileName),
    ListsS = lists:zip(List1S, List2S),
    lists:foldl(fun({Nr1, Nr2}, Acc) ->
        Acc + erlang:abs(Nr1-Nr2)
    end, 0, ListsS).


solve_2(FileName) ->
    {List1S, List2S} = read_sort_lists(FileName),
    ResultFun = fun
        ResultFun([], _, Acc) ->
            Acc;
        ResultFun([Nr1|Remaining], LList2, Acc) ->
            {Nr1Count, NewRemaining} = utils:count_elems_start([Nr1|Remaining]),
            {Nr2Count, NewLList2   } = utils:count_elems_sorted(Nr1, LList2),
            NewAcc = Acc + Nr1 * Nr2Count * Nr1Count,
            ResultFun(NewRemaining, NewLList2, NewAcc)
    end,
    ResultFun(List1S, List2S, 0).
