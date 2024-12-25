-module(day25).
-export([solve_1/1, solve_2/1]).

% Pirma dalis nebuvo sunki, o antros iš viso nebuvo.

% (aoc_2024@JuliusErisataT14.erisata.lt)1> day25:solve_1("priv/day25-PVZ.txt").
% 3
% (aoc_2024@JuliusErisataT14.erisata.lt)2> day25:solve_1("priv/day25.txt").
% 2835
% (aoc_2024@JuliusErisataT14.erisata.lt)3> timer:tc(fun() -> day25:solve_1("priv/day25.txt") end).
% {42970,2835}


read_inputs(FileName) ->
    Lines = utils:read_lines_no_new_line(FileName),
    KeyLockStrs = split(Lines),
    {Keys, Locks} = lists:foldl(fun(KeyLockStr, {AccKeys, AccLocks}) ->
        {KeyLockMatrix, {Rows, Cols}} = utils:get_char_matrix(KeyLockStr),
        %utils:print("-------------", []),
        %utils:print_char_matrix(KeyLockMatrix, {Rows, Cols} ),
        GetKeyOrLockFun = fun(First, Second) ->
            lists:map(fun(Column) ->
                FinalCount   = lists:foldl(fun(Row, Count) ->
                    Tile = maps:get({Row, Column}, KeyLockMatrix),
                    case {Tile, Count} of
                        {T, undefined} when T =:= First                 -> undefined;
                        {T, undefined} when T =:= Second                -> 0;
                        {T, C        } when T =:= Second, is_integer(C) -> Count+1
                    end
                end, undefined, lists:seq(1, Rows)),
                true =:= erlang:is_integer(FinalCount),
                FinalCount
            end, lists:seq(1, Cols))
        end,
        Size = Rows-2,
        case maps:get({1, 1}, KeyLockMatrix) of
            $# ->
                LockInv = GetKeyOrLockFun($#, $.),
                Lock = lists:map(fun(Inv) -> Size - Inv end, LockInv),
                %utils:print("LOCK: ~p", [Lock]),
                {AccKeys, [{Size, Lock} | AccLocks]};
            $. ->
                Key = GetKeyOrLockFun($., $#),
                %utils:print("KEY: ~p", [Key]),
                {[{Size, Key} | AccKeys], AccLocks}
        end
    end, {[], []}, KeyLockStrs).


split(Lines) ->
    split(Lines, [], []).

split([],           AccSingle, AccFinal) -> [AccSingle|AccFinal];
split([""|Lines],   AccSingle, AccFinal) -> split(Lines, [],               [AccSingle|AccFinal]);
split([Line|Lines], AccSingle, AccFinal) -> split(Lines, [Line|AccSingle], AccFinal            ).


count_fits(Keys, Locks) ->
    utils:list_map_sum(fun({Size, Key}) ->
        utils:list_filter_count(fun({S, Lock}) when S =:= Size ->
            KeyLock = lists:zip(Key, Lock),
            lists:all(fun({K, L}) ->
                K + L =< Size
            end, KeyLock)
        end, Locks)
    end, Keys).


solve_1(FileName) ->
    {Keys, Locks} = read_inputs(FileName),
    count_fits(Keys, Locks).


solve_2(FileName) ->
    ok.