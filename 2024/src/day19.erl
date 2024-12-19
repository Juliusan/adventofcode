-module(day19).
-export([solve_1/1, solve_2/1]).

% Iš pradžių parašiau, atrodo, gerą algoritmą, tik baisiai ilgai veikiantį.
% Tiesa, iš pradžių maniau, kad kažkas blogai pačiame algoritme, debuginausi,
% skubėjau, ilgokai užtrukau. Bet vėliau supratau, kad tarpinius rezultatus
% tiesiog reikia kešuoti. Pridėjau kešavimą ir gavau atsakymą. Antrą dalį iš
% karto supratau, kad bus nesunku padaryti. Tik kol atsipainiojau tarp
% akumuliatorių, truputį užtruko. Bet pirmu leidimu gavau gerą atsakymą. Ir
% gavęs atsakymą iš 15 skaitmenų supratau, kodėl pirma dalis taip ilgai sukosi.

% (aoc_2024@JuliusErisataT14.erisata.lt)1> day19:solve_1("priv/day19-PVZ.txt").
% 6
% (aoc_2024@JuliusErisataT14.erisata.lt)2> day19:solve_1("priv/day19.txt").
% 298
% (aoc_2024@JuliusErisataT14.erisata.lt)3> day19:solve_2("priv/day19-PVZ.txt").
% 16
% (aoc_2024@JuliusErisataT14.erisata.lt)4> day19:solve_2("priv/day19.txt").
% 572248688842069
% (aoc_2024@JuliusErisataT14.erisata.lt)5> timer:tc(fun() -> day19:solve_1("priv/day19.txt") end).
% {91103,298}
% (aoc_2024@JuliusErisataT14.erisata.lt)6> timer:tc(fun() -> day19:solve_2("priv/day19.txt") end).
% {57337,572248688842069}


read_inputs(FileName) ->
    [[PatternList], Designs] = utils:read_lines_no_new_line_to_elems(FileName, [
        fun(Line) -> string:split(Line, ", ", all) end,
        fun(Line) -> Line end
    ], "\n"),
    PatternMapNS = lists:foldl(fun([Char|Pattern], Acc) ->
        AccList = maps:get(Char, Acc, []),
        Acc#{Char => [[Char|Pattern]|AccList]}
    end, #{}, PatternList),
    PatternMap = maps:map(fun(_Key, List) -> lists:usort(List) end, PatternMapNS),
    {PatternMap, Designs}.


is_possible("", _, AccCount, AccCache) -> {AccCount+1, AccCache};
is_possible([Char|Design], Patterns, AccCount, AccCache) ->
    case maps:get([Char|Design], AccCache, undefined) of
        undefined ->
            PatternList = maps:get(Char, Patterns, []),
            {Count, NewAccCache} = is_possible([Char|Design], PatternList, Patterns, 0, AccCache),
            NewAccCount = AccCount+Count,
            {NewAccCount, NewAccCache#{[Char|Design] => Count}};
        Count when is_integer(Count) ->
            {AccCount+Count, AccCache}
    end.


is_possible(_Design, [], _, AccCount, AccCache) -> {AccCount, AccCache};
is_possible(Design, [Pattern|PatternList], Patterns, AccCount, AccCache) ->
    %utils:print("XXX ~p ~p ~p", [Design, Pattern, is_possible_pattern(Design, Pattern)]),
    %utils:wait_key_press(),
    case is_possible_pattern(Design, Pattern) of
        {true, RemDesign} ->
            {NewAccCount, NewAccCache} = is_possible(RemDesign, Patterns, AccCount, AccCache),
            is_possible(Design, PatternList, Patterns, NewAccCount, NewAccCache);
        false -> is_possible(Design, PatternList, Patterns, AccCount, AccCache)
    end.


is_possible_pattern(Design, []) -> {true, Design};
is_possible_pattern([Char|Design], [Char|Pattern]) -> is_possible_pattern(Design, Pattern);
is_possible_pattern(_Design, _Pattern) -> false.


count_possible(Designs, Patterns) ->
    {Count, _} = utils:list_foldl_sum(fun(Design, Acc) ->
        {Count, NewAcc} = is_possible(Design, Patterns, 0, Acc),
        % utils:print("POSSIBLE: ~p ~p", [Design, Count>0)]),
        % utils:wait_key_press(),
        case Count of
            0 -> {0, NewAcc};
            _ -> {1, NewAcc}
        end
    end, #{}, Designs),
    Count.


count_all(Designs, Patterns) ->
    {Count, _} = utils:list_foldl_sum(fun(Design, Acc) ->
        is_possible(Design, Patterns, 0, Acc)
    end, #{}, Designs),
    Count.


solve_1(FileName) ->
    {Patterns, Designs} = read_inputs(FileName),
    count_possible(Designs, Patterns).


solve_2(FileName) ->
    {Patterns, Designs} = read_inputs(FileName),
    count_all(Designs, Patterns).


% 266 too low