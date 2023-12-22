-module(day22_1).
-export([solve/1]).

% Nesudėtinga. Daugiausia programavimas. Pasimoviau ant to, kad padėtas naujas blokas nebūtinai yra
% aukščiausias krūvos taškas. Užtruko, kol tai supratau ir ištaisiau.

solve(FileName) ->
    Blocks = get_input(FileName),
    io:fwrite("XXX ~p~n", [Blocks]),
    {Pile, BlockCoords} = blocks_fall(Blocks),
    %io:fwrite("XXX ~p~n", [Pile]),
    %io:fwrite("XXX ~p~n", [BlockCoords]),
    FreeBlocks = count_free_blocks(Pile, BlockCoords),
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
    blocks_fall(Blocks, #{}, #{}, 1).

blocks_fall([], AccPile, AccBlocks, _Highest) ->
    {AccPile, AccBlocks};
    
blocks_fall([Block|Blocks], AccPile, AccBlocks, Highest) ->
    %io:fwrite("XXX ~p ~p~n", [Block,Blocks]),
    Level = find_start_level(Block, AccPile, Highest),
    {NewAccPile, NewAccBlocks, BlockHighest} = add_block(Block, Level, AccPile, AccBlocks),
    NewHighest = erlang:max(Highest, BlockHighest),
    blocks_fall(Blocks, NewAccPile, NewAccBlocks, NewHighest).
    

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


add_block({Name, _, _} = Block, Level, Pile, Blocks) ->
    Coords = get_coordinates(Block, Level),
    NewPile = lists:foldl(fun(Coord, AccPile) ->
        AccPile#{Coord => Name}
    end, Pile, Coords),
    NewBlocks = Blocks#{Name => Coords},
    Highest = highest_z(Coords),
    {NewPile, NewBlocks, Highest}.
    
    
highest_z([{_, _, Z}            ]) -> Z;
highest_z([{_, _, Z},{_, _, Z}|_]) -> Z;
highest_z(Coords                 ) -> {_,_,Z} = lists:last(Coords), Z.


count_free_blocks(Pile, Blocks) ->
    {Supports, Supported} = fill_support(Pile, Blocks),
    %io:fwrite("XXX ~p~n", [Supports]),
    %io:fwrite("XXX ~p~n", [Supported]),
    count_blocks(Supports, Supported).
    
    
fill_support(Pile, Blocks) ->
    maps:fold(fun(Name, Coords, {AccSupports, AccSupported}) ->
        AllNameSupported = lists:foldl(fun
            ({_X, _Y, 1}, AccS) ->
                AccS;
            ({X, Y, Z}, AccS) ->
                case maps:find({X, Y, Z-1}, Pile) of
                    {ok,  Name} -> AccS;
                    {ok, SName} -> [SName|AccS];
                    error       -> AccS
                end
        end, [], Coords),
        NameSupported = lists:usort(AllNameSupported),
        NewAccSupported = AccSupported#{Name => NameSupported},
        NewAccSupports = lists:foldl(fun(SName, AccS) ->
            case maps:find(SName, AccS) of
                {ok, Value} -> AccS#{SName => [Name | Value]};
                error       -> AccS#{SName => [Name]}
            end
        end, AccSupports, NameSupported),
        {NewAccSupports, NewAccSupported}
    end, {#{}, #{}}, Blocks).
    
    
count_blocks(Supports, Supported) ->
    lists:foldl(fun(Name, AccCount) ->
        case maps:find(Name, Supports) of
            {ok, SupportedBlocks} ->
                %io:fwrite("XXX ~p -> ~p ", [Name, SupportedBlocks]),
                WillFall = lists:any(fun(SBlock) ->
                    #{SBlock := SBlockSupports} = Supported,
                    %io:fwrite("~p:~p ", [SBlock, SBlockSupports]),
                    case SBlockSupports of
                        [_,_|_] -> false;
                        [_|_]   -> true
                    end
                end, SupportedBlocks),
                %io:fwrite("~p~n", [WillFall]),
                case WillFall of
                    true  -> AccCount;
                    false -> AccCount+1
                end;
            error ->
                %io:fwrite("XXX ~p -> [] false~n", [Name]),
                AccCount+1
        end
    end, 0, maps:keys(Supported)).
   
    
numbers_str_to_list(NumbersStr) ->
    NumbersStrList = string:split(NumbersStr, ",", all),
    lists:filtermap(fun
        ("")        -> false;
        (NumberStr) -> {true, erlang:list_to_integer(NumberStr)}
    end, NumbersStrList).


trim_ending_newline(Str) ->
    string:sub_string(Str, 1, string:len(Str) - 1).

