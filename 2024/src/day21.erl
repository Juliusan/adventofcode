-module(day21).
-export([solve_1/1, solve_2/1]).

% Iš pradžių padariau be kešavimo - tiesiog ėjimų skaičiavimą. Turėjau nuojautą,
% kad antra dalis tiesiog turės daugiau lygių. Bet turėjau išeitį - pridėsiu
% kešavimą ir tiek. Iš pradžių kešavau ėjimų sekas, vis pridėdamas po vieną
% ėjimą prie jau sukešuotos sekos. Po to sugalvojau kešuoti blokų (ėjimų seka
% iki `activate`) sekas. Bet man vis buvo reikalinga saugoti realias ėjimų (ar
% blokų) sekas, tai jau 5 lygyje jos pasidarydavo tokios didelės, kad nelabai
% ką ir suskaičiuodavo. Pastebėjau, kad programa stringa ant tų sekų surinkimo
% iš opcijų. Galvojau, gal kažkaip opcijas geriau saugoti. Tada galvojau, gal
% iš dviejų variantų visada geresnis yra kuris nors vienas. Ne tiek daug tų
% variantų su realiomis opcijomis yra. Bandžiau spėlioti. Ir vis tiek iki 25
% lygio sunkiai nueidavo. Galiausiai pastebėjau, kad kešuoju ne rekursijos
% žingsnį, o jos rezultatą. Kai tik perrašiau, kad kešuočiau žingsnį, nebeliko
% beprotiškų sąrašų saugojimo poreikio ir viskas pradėjo suktis milisekundėmis.
% Užtrukau žymiai ilgiau, negu norėčiau pripažinti.

% (aoc_2024@JuliusErisataT14.erisata.lt)1> day21:solve_1("priv/day21-PVZ.txt").
% 126384
% (aoc_2024@JuliusErisataT14.erisata.lt)2> day21:solve_1("priv/day21.txt").
% 202648
% (aoc_2024@JuliusErisataT14.erisata.lt)3> day21:solve_2("priv/day21-PVZ.txt").
% 154115708116294
% (aoc_2024@JuliusErisataT14.erisata.lt)4> day21:solve_2("priv/day21.txt").
% 248919739734728
% (aoc_2024@JuliusErisataT14.erisata.lt)5> timer:tc(fun() -> day21:solve_1("priv/day21.txt") end).
% {638,202648}
% (aoc_2024@JuliusErisataT14.erisata.lt)6> timer:tc(fun() -> day21:solve_2("priv/day21.txt") end).
% {1464,248919739734728}


read_inputs(FileName) ->
    utils:read_lines_no_new_line_to_elems(FileName, fun(Line) ->
        lists:map(fun
            ($A  ) -> activate;
            (Char) -> Char
        end, Line)
    end).


digit_keypad() ->
    Lines = [
        "789",
        "456",
        "123",
        "#0A"
    ],
    {MapChars, _Dimensions} = utils:get_char_matrix(Lines),
    Map = maps:map(fun
        (_Key, $A  ) -> activate;
        (_Key, $#  ) -> empty;
        (_Key, Char) -> Char
    end, MapChars),
    utils:map_reverse(Map).


directions_keypad() ->
    Lines = [
        "#^A",
        "<v>"
    ],
    {MapChars, _Dimensions} = utils:get_char_matrix(Lines),
    Map = maps:map(fun
        (_Key, $A) -> activate;
        (_Key, $^) -> up;
        (_Key, $<) -> left;
        (_Key, $v) -> down;
        (_Key, $>) -> right;
        (_Key, $#) -> empty
    end, MapChars),
    utils:map_reverse(Map).


code_to_block_options(Code, KeyPad) ->
    code_to_block_options(Code, KeyPad, activate, []).

code_to_block_options([], _, _, Acc) ->
    lists:reverse(Acc);

code_to_block_options([NextCel|Code], KeyPad, CurrCel, Acc) ->
    {CurrRow, CurrCol}   = maps:get(CurrCel, KeyPad),
    {NextRow, NextCol}   = maps:get(NextCel, KeyPad),
    {EmptyRow, EmptyCol} = maps:get(empty,   KeyPad),
    Hor = case NextCol - CurrCol of
        CDiff when CDiff > 0 -> lists:duplicate(CDiff, right);
        0                    -> [];
        CDiff when CDiff < 0 -> lists:duplicate(-CDiff, left)
    end,
    Ver = case NextRow - CurrRow of
        RDiff when RDiff > 0 -> lists:duplicate(RDiff, down);
        0                  -> [];
        RDiff when RDiff < 0 -> lists:duplicate(-RDiff, up)
    end,
    Moves = case {Hor, Ver, CurrRow, CurrCol, NextRow, NextCol} of
        {[],  _, _,        _,        _,        _       } -> [Ver ++ [activate]];
        { _, [], _,        _,        _,        _       } -> [Hor ++ [activate]];
        { _,  _, EmptyRow, _,        _,        EmptyCol} -> [Ver ++ Hor ++ [activate]];
        { _,  _, _,        EmptyCol, EmptyRow, _       } -> [Hor ++ Ver ++ [activate]];
        { _,  _, _,        _,        _,        _       } -> [Hor ++ Ver ++ [activate], Ver ++ Hor ++ [activate]]
    end,
    code_to_block_options(Code, KeyPad, NextCel, [Moves|Acc]).


block_otpions_to_blocks(BlockOpts) -> block_otpions_to_blocks(BlockOpts, [], []).
block_otpions_to_blocks([], AccBlocks, AccFinalBlocks) -> [lists:reverse(AccBlocks)|AccFinalBlocks];
block_otpions_to_blocks([BlockOpt|BlockOpts], AccBlocks, AccFinalBlocks) ->
    lists:foldl(fun(Block, Acc) ->
        block_otpions_to_blocks(BlockOpts, [Block|AccBlocks], Acc)
    end, AccFinalBlocks, BlockOpt).


find_shortest_block(Block, DirectionKeyPad, 1, Cache) ->
    case maps:get({Block, 1}, Cache, undefined) of
        undefined ->
            NewBlockOpts = code_to_block_options(Block, DirectionKeyPad),
            NewBlockList = block_otpions_to_blocks(NewBlockOpts),
            [FirstBlocks|_] = NewBlockList,
            BlocksLengthFun = fun(Blocks) -> utils:list_map_sum(fun erlang:length/1, Blocks) end,
            Length = BlocksLengthFun(FirstBlocks),
            true = lists:all(fun(Blocks) -> Length =:= BlocksLengthFun(Blocks) end, NewBlockList), % Check
            {Length, Cache#{{Block, 1} => {Length, NewBlockList}}};
        {Length, _BlockList} ->
            {Length, Cache}
    end;

find_shortest_block(Block, KeyPad, Count, Cache) when Count > 1 ->
    case maps:get({Block, Count}, Cache, undefined) of
        undefined ->
            {StepLength, NewCache1} = find_shortest_block(Block, KeyPad, 1, Cache),
            {StepLength, StepBlockList} = maps:get({Block, 1}, NewCache1),
            {NewLength, NewCache2} = find_shortest_block_list(StepBlockList, KeyPad, Count-1, NewCache1),
            NewCache = NewCache2#{{Block, Count} => NewLength},
            {NewLength, NewCache};
        Length ->
            {Length, Cache}
    end.


find_shortest_blocks(Blocks, KeyPad, Count, Cache) ->
    utils:list_foldl_sum(fun(Block, AccCache) ->
        find_shortest_block(Block, KeyPad, Count, AccCache)
    end, Cache, Blocks).


find_shortest_block_list(BlockList, KeyPad, Count, Cache) ->
    lists:foldl(fun(Blocks, {AccShortest, AccCache}) ->
        {Shortest, NewAccCache} = find_shortest_blocks(Blocks, KeyPad, Count, AccCache),
        NewShortest = lists:min([Shortest, AccShortest]),
        {NewShortest, NewAccCache}
    end, {infinity, Cache}, BlockList).



find_shortest_code(Code, Count, DigitKeyPad, DirectionKeyPad, Cache) ->
    BlockOpts = code_to_block_options(Code, DigitKeyPad),
    BlockList = block_otpions_to_blocks(BlockOpts),
    find_shortest_block_list(BlockList, DirectionKeyPad, Count, Cache).


find_shortest_codes(Codes, Count) ->
    DigitKeyPad     = digit_keypad(),
    DirectionKeyPad = directions_keypad(),
    {Complexity, _} = utils:list_foldl_sum(fun(Code, AccCache) ->
        {Shortest, NewAccCache} = find_shortest_code(Code, Count, DigitKeyPad, DirectionKeyPad, AccCache),
        {Int, [activate]} = utils:get_integer(Code),
        %utils:print("MIN ~p DIGIT ~p", [Shortest, Int]),
        {Shortest * Int, NewAccCache}
    end, #{}, Codes),
    Complexity.


solve(FileName, Count) ->
    Codes = read_inputs(FileName),
    find_shortest_codes(Codes, Count).


solve_1(FileName) ->
    solve(FileName, 2).


solve_2(FileName) ->
    solve(FileName, 25).
