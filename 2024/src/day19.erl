-module(day19).
-export([solve_1/1, solve_2/1]).

% Iš pradžių parašiau, paleidau ir gavau per mažą atsakymą. Tada supratau savo
% klaidą, pataisiau ir, atrodo, algoritms buvo geras, tik baisiai ilgai
% veikiantis. Tiesa, iš pradžių maniau, kad kažkas blogai pačiame algoritme,
% debuginausi, skubėjau, ilgokai užtrukau. Bet vėliau supratau, kad tiesiog
% reikia kešuoti tarpinius rezultatus. Pridėjau kešavimą ir gavau atsakymą.
% Antrą dalį iš karto supratau, kad bus nesunku padaryti. Tik kol atsipainiojau
% tarp akumuliatorių, truputį užtruko. Bet pirmu leidimu gavau gerą atsakymą.
% Ir gavęs atsakymą iš 15 skaitmenų supratau, kodėl pirma dalis taip ilgai
% sukosi. Na o galiausiai prisėdau sutvarkyti kodą, nes buvo jau baisiai
% suveltas. Ir ne tik jį pagražinau, bet dar ir sutrumpinau. Išmečiau vieną
% lygį. Taip ir nesuprantu, kodėl pirminė mano idėja buvo tokia paini. Matyt,
% jeigu iš karto būčiau pradėjęs nuo antros dalies, tai tiek nebūčiau privyniojęs.

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
    PatternMap = lists:foldl(fun([P1|_] = Pattern, Acc) ->
        P1Patterns = maps:get(P1, Acc, []),
        Acc#{P1 => [Pattern|P1Patterns]}
    end, #{}, PatternList),
    %PatternMap = maps:map(fun(_Key, List) -> lists:usort(List) end, PatternMapNS), % To ease debugging; not necessary
    {PatternMap, Designs}.


count_possible([],              _,        Cache) -> {1, Cache};
count_possible([D1|_] = Design, Patterns, Cache) ->
    D1Patterns = maps:get(D1, Patterns, []),
    case maps:get(Design, Cache, undefined) of
        undefined ->
            {Count, NewCache} = utils:list_foldl_sum(fun(Pattern, AccCache) ->
                case apply_pattern(Design, Pattern) of
                    {ok, RemainingDesign} -> count_possible(RemainingDesign, Patterns, AccCache);
                    error                 -> {0, AccCache}
                end
            end, Cache, D1Patterns),
            {Count, NewCache#{Design => Count}};
        Count when is_integer(Count) ->
            {Count, Cache}
    end.


apply_pattern(Design, []) -> {ok, Design};
apply_pattern([Char|Design], [Char|Pattern]) -> apply_pattern(Design, Pattern);
apply_pattern(_Design, _Pattern) -> error.


count_possible_designs(Designs, Patterns) ->
    {Count, _} = utils:list_foldl_count(fun(Design, Acc) ->
        {Count, NewAcc} = count_possible(Design, Patterns, Acc),
        % utils:print("POSSIBLE: ~p ~p", [Design, Count>0)]),
        % utils:wait_key_press(),
        {Count>0, NewAcc}
    end, #{}, Designs),
    Count.


count_possible(Designs, Patterns) ->
    {Count, _} = utils:list_foldl_sum(fun(Design, Acc) ->
        count_possible(Design, Patterns, Acc)
    end, #{}, Designs),
    Count.


solve_1(FileName) ->
    {Patterns, Designs} = read_inputs(FileName),
    count_possible_designs(Designs, Patterns).


solve_2(FileName) ->
    {Patterns, Designs} = read_inputs(FileName),
    count_possible(Designs, Patterns).
