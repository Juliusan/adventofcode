-module(day22_2).
-export([solve/1]).

% Dariau paprasčiausiu būdu ir tuo pat metu galvojau, kaip galima būtų paomtimizuoti kodą.
% Pasirodo, neprireikė. Sukasi apie 5 sekundes, bet tai yra pakenčiama.

solve(FileName) ->
    Blocks = get_input(FileName),
    %io:fwrite("XXX ~p~n", [Blocks]),
    {Pile, BlockCoords, _} = blocks_fall(Blocks),
    %io:fwrite("XXX ~p~n", [Pile]),
    %io:fwrite("XXX ~p~n", [BlockCoords]),
    FreeBlocks = count_brick_fall(Pile, BlockCoords),
    FreeBlocks.
        
    
get_input(FileName) ->
    {ok, File} = file:open(FileName, [read]),
    Modules = get_input_snapshot(File, [], 1),
    ok = file:close(File),
    Modules.

    
get_input_snapshot(File, AccSnapshot, NextName) ->
    case file:read_line(File) of
        eof ->
            lists:sort(fun({_, {_,_,ZFrom1}, _}, {_, {_,_,ZFrom2}, _}) -> ZFrom1 < ZFrom2 end, AccSnapshot);
        {ok, Line} ->
            SnapshotLine = trim_ending_newline(Line),
            {From, To} = get_input_snapshot_line(SnapshotLine),
            Result = {NextName, From, To},
            get_input_snapshot(File, [Result|AccSnapshot], NextName+1)
    end.
    
    
get_input_snapshot_line(Line) ->
    [FromStr, ToStr] = string:split(Line, "~"),
    [XFrom, YFrom, ZFrom] = numbers_str_to_list(FromStr),
    [XTo,   YTo,   ZTo  ] = numbers_str_to_list(ToStr),
    {{XFrom, YFrom, ZFrom}, {XTo, YTo, ZTo}}.


blocks_fall(Blocks) ->
    blocks_fall(Blocks, #{}, #{}, 1, 0).

blocks_fall([], AccPile, AccBlocks, _Highest, AccFell) ->
    {AccPile, AccBlocks, AccFell};
    
blocks_fall([Block|Blocks], AccPile, AccBlocks, Highest, AccFell) ->
    %io:fwrite("XXX ~p ~p~n", [Block,Blocks]),
    Level = find_start_level(Block, AccPile, Highest),
    {NewAccPile, NewAccBlocks, BlockHighest, DidFall} = add_block(Block, Level, AccPile, AccBlocks),
    NewHighest = erlang:max(Highest, BlockHighest),
    NewAccFell = AccFell+DidFall,
    blocks_fall(Blocks, NewAccPile, NewAccBlocks, NewHighest, NewAccFell).
    

find_start_level(_Block, _AccPile, 0) ->
    1;
    
find_start_level(Block, AccPile, Level) ->
    Coords = get_coordinates(Block, Level),
    IsClash = lists:any(fun(Coord) -> 
        case maps:find(Coord, AccPile) of
            {ok, _Value} -> true;
            error        -> false
        end
    end, Coords),
    case IsClash of
        true  -> Level+1;
        false -> find_start_level(Block, AccPile, Level-1)
    end.
    

get_coordinates({_Name, {XFrom, Y, Z}, {XTo, Y, Z}}, Level) -> [ {X, Y, Level} || X <- lists:seq(XFrom, XTo            ) ];
get_coordinates({_Name, {X, YFrom, Z}, {X, YTo, Z}}, Level) -> [ {X, Y, Level} || Y <- lists:seq(YFrom, YTo            ) ];
get_coordinates({_Name, {X, Y, ZFrom}, {X, Y, ZTo}}, Level) -> [ {X, Y, Z    } || Z <- lists:seq(Level, Level+ZTo-ZFrom) ].


add_block({Name, From, To} = Block, Level, Pile, Blocks) ->
    Coords = get_coordinates(Block, Level),
    DidFall = case {erlang:hd(Coords), lists:last(Coords)} of
        {From, To} -> 0;
        _          -> 1
    end,
    NewPile = lists:foldl(fun(Coord, AccPile) ->
        AccPile#{Coord => Name}
    end, Pile, Coords),
    NewBlocks = Blocks#{Name => Coords},
    Highest = highest_z(Coords),
    {NewPile, NewBlocks, Highest, DidFall}.
    
    
highest_z([{_, _, Z}            ]) -> Z;
highest_z([{_, _, Z},{_, _, Z}|_]) -> Z;
highest_z(Coords                 ) -> {_,_,Z} = lists:last(Coords), Z.


count_brick_fall(_Pile, BlockCoords) ->
    Blocks = maps:map(fun(Name, Coords) ->
        {Name, erlang:hd(Coords), lists:last(Coords)}
    end, BlockCoords),
    maps:fold(fun(Name, _, AccCount) ->
        %NewPile = lists:foldl(fun(Coord, AccPile) ->
        %    maps:remove(Coord, AccPile)
        %end, Pile, BlockCoords),
        NewBlocks = maps:remove(Name, Blocks),
        SortedBlocks = lists:sort(fun({_, {_,_,ZFrom1}, _}, {_, {_,_,ZFrom2}, _}) -> ZFrom1 < ZFrom2 end, maps:values(NewBlocks)),
        {_, _, BlocksFall} = blocks_fall(SortedBlocks),
        BlocksFall + AccCount
    end, 0, Blocks).


numbers_str_to_list(NumbersStr) ->
    NumbersStrList = string:split(NumbersStr, ",", all),
    lists:filtermap(fun
        ("")        -> false;
        (NumberStr) -> {true, erlang:list_to_integer(NumberStr)}
    end, NumbersStrList).


trim_ending_newline(Str) ->
    string:sub_string(Str, 1, string:len(Str) - 1).

