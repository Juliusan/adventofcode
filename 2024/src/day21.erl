-module(day21).
-export([solve_1/1, solve_2/1]).

% Niekaip nesugalvoju, kaip optimizuoti...

% (aoc_2024@JuliusErisataT14.erisata.lt)1> day21:solve_1("priv/day21-PVZ.txt").
% 126384
% (aoc_2024@JuliusErisataT14.erisata.lt)2> day21:solve_1("priv/day21.txt").
% 202648
% (aoc_2024@JuliusErisataT14.erisata.lt)3> timer:tc(fun() -> day21:solve_1("priv/day21.txt") end).
% {658,202648}


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
    reverse_map(Map).


reverse_map(Map) ->
    maps:fold(fun(Key, Value, Acc) -> Acc#{Value => Key} end, #{}, Map).


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
    reverse_map(Map).


enter_code(Code, KeyPad) ->
    enter_code(Code, KeyPad, activate, []).


enter_code([], _, _, Acc) -> lists:reverse(Acc);
enter_code([NextCel|Code], KeyPad, CurrCel, Acc) ->
    {CurrRow, CurrCol}   = maps:get(CurrCel,  KeyPad),
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
    enter_code(Code, KeyPad, NextCel, [Moves|Acc]).


% find_moves_in_cache([First|Moves], Cache) ->
%     {Shortest, ShortestMoves} = maps:get([First], Cache),
%     find_moves_in_cache([First], Moves, Shortest, ShortestMoves, Cache).


% find_moves_in_cache(AllMoves, [], AccShortest, AccShortestMoves, _Cache) ->
%     {AllMoves, [], AccShortest, AccShortestMoves};

% find_moves_in_cache(FirstMoves, [CutMove|LastMoves], AccShortest, AccShortestMoves, Cache) ->
%     FirstMovesCut = FirstMoves ++ [CutMove],
%     case maps:get(FirstMovesCut, Cache, undefined) of
%         undefined                       -> {FirstMoves, [CutMove|LastMoves], AccShortest, AccShortestMoves};
%         {NewShortest, NewShortestMoves} -> find_moves_in_cache(FirstMovesCut, LastMoves, NewShortest, NewShortestMoves, Cache)
%     end.


% enter_directions([], _KeyPad, _LastMove, AccShortest, AccMoves, AccFinalShortest, AccFinalMoves, Cache) ->
%     %utils:print("XXX ~p ~p ~p ~p", [AccShortest, AccMoves, AccFinalShortest, AccFinalMoves]),
%     AccMovesFlat = get_moves(lists:reverse(AccMoves)),
%     %utils:print("XXX2 ~p", [AccMovesFlat]),
%     case {AccShortest, AccFinalShortest} of
%         _ when AccShortest  <  AccFinalShortest -> {AccShortest,      AccMovesFlat,                  Cache};
%         _ when AccShortest =:= AccFinalShortest -> {AccShortest,      AccFinalMoves ++ AccMovesFlat, Cache};
%         _ when AccShortest  >  AccFinalShortest -> {AccFinalShortest, AccFinalMoves,                 Cache}
%     end;

% enter_directions(Moves, KeyPad, LastMove, AccShortest, AccMoves, AccFinalShortest, AccFinalMoves, Cache) ->
%     LastMoveCache = maps:get(LastMove, Cache, #{}),
%     {MovesFound, NewMovesNotCut, Shortest, ShortestMoves} = find_moves_in_cache(Moves, LastMoveCache),
%     case NewMovesNotCut of
%         [] ->
%             NewAccShortest = AccShortest + Shortest,
%             NewAccMoves = [ShortestMoves | AccMoves ],
%             enter_directions([], KeyPad, undefined, NewAccShortest, NewAccMoves, AccFinalShortest, AccFinalMoves, Cache);
%         [CutMove|NewMoves] ->
%             {SingleShortest, SingleShortestMoves} = maps:get([CutMove], maps:get(lists:last(MovesFound), Cache)),
%             NewShortest = SingleShortest + Shortest,
%             NewShortestMoves = [ lists:append(SM, SSM) || SM <- ShortestMoves, SSM <- SingleShortestMoves ],
%             NewAccShortest = AccShortest + NewShortest,
%             NewAccMoves = [NewShortestMoves | AccMoves ],
%             NewLastMoveCache = LastMoveCache#{MovesFound ++ [CutMove] => {NewShortest, NewShortestMoves}},
%             NewCache = Cache#{LastMove => NewLastMoveCache},
%             %utils:print("NEW CACHe ~p", [NewCache]),
%             enter_directions(NewMoves, KeyPad, CutMove, NewAccShortest, NewAccMoves, AccFinalShortest, AccFinalMoves, NewCache)
%     end.


% enter_directions(MovesList, KeyPad, Shortest, Moves, Cache) ->
%     lists:foldl(fun(Ms, {AccShortest, AccMoves, AccCache}) ->
%         %utils:print("XXX CACHE ~p", [utils:map_map_sum(fun(_, Map2) -> maps:size(Map2) end, AccCache)]),
%         enter_directions(Ms, KeyPad, activate, 0, [[[]]], AccShortest, AccMoves, AccCache)
%     end, {Shortest, Moves, Cache}, MovesList).

% pre_seed_cache(NumberKeyPad) ->
%     Keys = [up, right, down, left, activate],
%     lists:foldl(fun(From, Acc1) ->
%         MoveMap = lists:foldl(fun(To, Acc2) ->
%             Moves = get_moves(enter_code([To], NumberKeyPad, From, [])),
%             [Move|_] = Moves,
%             Acc2#{[To] => {erlang:length(Move), Moves}}
%         end, #{}, Keys),
%         Acc1#{From => MoveMap}
%     end, #{}, Keys).


% enter_code_robot(MovesList, _, 0, Cache) ->
%     {MovesList, Cache};

% enter_code_robot(MovesList, NumberKeyPad, Count, Cache) when Count > 0 ->
%     {_, NewMovesList, NewCache} = enter_directions(MovesList, NumberKeyPad, infinity, [], Cache),
%     %utils:print("LEVEL ~p", [Count]),
%     %print_moves(NewMovesList),
%     enter_code_robot(NewMovesList, NumberKeyPad, Count-1, NewCache).


% enter_codes(Code, Count, Cache) ->
%     DigitKeyPad = digit_keypad(),
%     NumberKeyPad = number_keypad(),
%     %utils:print("~p", [DigitKeyPad]),
%     Sequence = enter_code(Code, DigitKeyPad),
%     MovesList = get_moves(Sequence),
%     %print_moves(MovesList),
%     enter_code_robot(MovesList, NumberKeyPad, Count, Cache).


get_moves(MoveOpts) ->
    get_moves(MoveOpts, [], []).

get_moves([], AccMove, AccMoves) -> [lists:append(lists:reverse(AccMove))|AccMoves];
get_moves([MoveOpt|MoveOpts], AccMove, AccMoves) ->
    lists:foldl(fun(Move, Acc) ->
        get_moves(MoveOpts, [Move|AccMove], Acc)
    end, AccMoves, MoveOpt).


% print_moves(Moves) ->
%     lists:foreach(fun(Move) ->
%         MoveStr = lists:map(fun
%             (up      ) -> $^;
%             (activate) -> $A;
%             (left    ) -> $<;
%             (down    ) -> $v;
%             (right   ) -> $>
%         end, Move),
%         utils:print("~p", [MoveStr])
%     end, Moves).

get_block(Moves) ->
    get_block(Moves, []).

get_block([activate|Moves], Acc) -> {lists:reverse([activate|Acc]), Moves};
get_block([First|Moves], Acc) -> get_block(Moves, [First|Acc]).


to_blocks(Moves) -> to_blocks(Moves, []).
to_blocks([], Acc) -> lists:reverse(Acc);
to_blocks(Moves, Acc) ->
    {Block, OtherMoves} = get_block(Moves),
    to_blocks(OtherMoves, [Block|Acc]).

count_block_robot(Block, DirectionKeyPad, 1, Cache) ->
    case maps:get({Block, 1}, Cache, undefined) of
        undefined ->
            Sequence = enter_code(Block, DirectionKeyPad),
            %utils:print("SEQUENCE ~p", [Sequence]),
            MoveList = get_moves(Sequence),
            [Moves|_] = MoveList,
            Length = erlang:length(Moves),
            true = lists:all(fun(M) -> Length =:= erlang:length(M) end, MoveList), % Check
            {Length, MoveList, Cache#{{Block, 1} => {Length, [Moves]}}};%MoveList}}};
        {Length, MoveList} ->
            {Length, MoveList, Cache}
    end;

count_block_robot(Block, DirectionKeyPad, Count, Cache) when Count > 1 ->
    %utils:print("CACHE: ~p", [maps:size(Cache)]),
    case maps:get({Block, Count}, Cache, undefined) of
        undefined ->
            {_PrevLength, PrevMoveList, NewCache1} = count_block_robot(Block, DirectionKeyPad, Count-1, Cache),
            {FinalLength, StepMoveOpts, NewCache2} = count_code_robot(PrevMoveList, DirectionKeyPad, 1, NewCache1),
            %utils:print("XXX ~p", [StepMoveOpts]),
            NewMoveList = lists:append(lists:map(fun get_moves/1, StepMoveOpts)),
            NewCache = NewCache2#{{Block, Count} => {FinalLength, NewMoveList}},
            {FinalLength, NewMoveList, NewCache};
        {Length, BlockMoves} ->
            {Length, BlockMoves, Cache}
    end.


count_blocks_robot([], _, _, AccLength, AccMoveOpts, Cache) ->
    {AccLength, lists:reverse(AccMoveOpts), Cache};

count_blocks_robot([Block|Blocks], DirectionKeyPad, Count, AccLength, AccMoveOpts, Cache) ->
    {BlockLength, BlockMoveList, NewCache} = count_block_robot(Block, DirectionKeyPad, Count, Cache),
    count_blocks_robot(Blocks, DirectionKeyPad, Count, AccLength + BlockLength, [BlockMoveList|AccMoveOpts], NewCache).


count_code_robot(MoveList, DirectionKeyPad, Count, Cache) ->
    lists:foldl(fun(Moves, {AccShortest, AccMoveOpts, AccCache}) ->
        Blocks = to_blocks(Moves),
        {Shortest, NewMoveOpts, NewAccCache} = count_blocks_robot(Blocks, DirectionKeyPad, Count, 0, [], AccCache),
        case {Shortest, AccShortest} of
            _ when Shortest  <  AccShortest -> {Shortest,    [NewMoveOpts],               NewAccCache};
            _ when Shortest =:= AccShortest -> {AccShortest, [NewMoveOpts | AccMoveOpts], NewAccCache};
            _ when Shortest  >  AccShortest -> {AccShortest, AccMoveOpts,                 NewAccCache}
        end
    end, {infinity, [], Cache}, MoveList).



count_code(Code, Count, DigitKeyPad, DirectionKeyPad, Cache) ->
    Sequence = enter_code(Code, DigitKeyPad),
    MoveList = get_moves(Sequence),
    %print_moves(MovesList),
    count_code_robot(MoveList, DirectionKeyPad, Count, Cache).


count_codes(Codes, Count) ->
    DigitKeyPad     = digit_keypad(),
    DirectionKeyPad = directions_keypad(),
    {Sum, C} = utils:list_foldl_sum(fun(Code, AccCache) ->
        {Shortest, _, NewAccCache} = count_code(Code, Count, DigitKeyPad, DirectionKeyPad, AccCache),
        %print_moves(Moves),
        %MoveLengths = lists:map(fun erlang:length/1, Moves),
        {Int, [activate]} = utils:get_integer(Code),
        %utils:print("MIN ~p DIGIT ~p", [Shortest, Int]),
        {Shortest * Int, NewAccCache}
    end, #{}, Codes),
    %utils:print("XXX ~p", [C]),
    %utils:print("XXX ~p~nXXX ~p~nXXX ~p", [maps:get({[up,up,left,left,activate], 1}, C), maps:get({[up,up,left,left,activate], 2}, C), maps:get({[up,up,left,left,activate], 3}, C)]),
    Sum.


solve(FileName, Count) ->
    Codes = read_inputs(FileName),
    count_codes(Codes, Count).


solve_1(FileName) ->
    solve(FileName, 2).


solve_2(FileName) ->
    solve(FileName, 25).
