-module(day18_2).
-export([solve/1]).

% Supratau, kad 100k x 100k žemėlapį sunkiai sutalpinsiu į atmintį, o ir kas iš to - matyt, vis tiek ilgai skaičiuos.
% Nusprendžiau patobulinti algoritmą. Galvojau, pasimesiu su tais indeksais ir ilgai debuginsiu. Bet, pasirodo,
% viskas buvo geriau, nei aš galvojau. Prasisuka labai greitai net ir su tikrais duomenimis (~16 ms).

solve(FileName) ->
    Input = get_input(FileName),
    {Size, Start} = get_size_and_start(Input),
    %io:fwrite("XXX ~p ~p~n", [Size, Start]),
    Map = get_map(Size, Start, Input),
    %io:fwrite("XXX MAP ~p~n", [Map]),
    Result = count_inside(Map),
    Result.
    
    
get_input(FileName) ->
    {ok, File} = file:open(FileName, [read]),
    Map = get_input(File, []),
    ok = file:close(File),
    Map.

    
get_input(File, AccInput) ->
    case file:read_line(File) of
        eof ->
            lists:reverse(AccInput);
        {ok, Line} ->
            %io:fwrite("XXX ~p: ", [Line]),
            InputLine = trim_ending_newline(Line),
            [_DirectionStr, _LengthStr, Colour] = string:split(InputLine, " ", all),
            [$(, $#, D1, D2, D3, D4, D5, D6, $)] = Colour,
            Direction = case D6 of
                $0 -> right;
                $1 -> down;
                $2 -> left;
                $3 -> up
            end,
            Length = erlang:list_to_integer([D1, D2, D3, D4, D5], 16),
            %io:fwrite("~p~n", [MapNode]),
            get_input(File, [{Direction, Length, Colour} | AccInput])
    end.
    
    
get_size_and_start(Input) ->
    {{RowS, ColS}, {RowE, ColE}} = get_dimensions({1, 1}, {1, 1}, {1, 1}, Input),
    Size = {RowE - RowS + 1, ColE - ColS + 1},
    Start = {2 - RowS, 2 - ColS},
    {Size, Start}.
    
    
get_dimensions(_,          Start,          End, []                          ) -> {Start, End};
get_dimensions({Row, Col}, Start, {RowE, ColE}, [{right, Length, _} | Input]) -> get_dimensions({Row, Col+Length}, Start, {RowE, erlang:max(Col+Length, ColE)}, Input);
get_dimensions({Row, Col}, Start, {RowE, ColE}, [{down,  Length, _} | Input]) -> get_dimensions({Row+Length, Col}, Start, {erlang:max(Row+Length, RowE), ColE}, Input);
get_dimensions({Row, Col}, {RowS, ColS},   End, [{left,  Length, _} | Input]) -> get_dimensions({Row, Col-Length}, {RowS, erlang:min(Col-Length, ColS)},   End, Input);
get_dimensions({Row, Col}, {RowS, ColS},   End, [{up,    Length, _} | Input]) -> get_dimensions({Row-Length, Col}, {erlang:min(Row-Length, RowS), ColS},   End, Input).


get_map({Rows, _}, Start, Input) ->
    InitMap = [{1, Rows, []}],
    [First | _] = Input,
    Input2 = Input ++ [First],
    {Map, Start} = fill_map(Start, Input2, InitMap),
    %io:fwrite("XXX MAP ~p~n", [Map]),
    Filled = fill_gaps(Map),
    %io:fwrite("XXX FILLED ~p~n", [Map]),
    Filled.
    %lists:foldl(fun(Row, AccMap) ->
    %    lists:foldl(fun(Col, AccM) ->
    %        case get_cell({Row, Col}, AccM) of
    %            false -> fill_cell({Row, Col}, $., AccM);
    %            _     -> AccM
    %        end
    %    end, AccMap, lists:seq(1, Cols))
    %end, Map, lists:seq(1, Rows)).

    
fill_map(Cell, [_], AccMap) -> {AccMap, Cell};
    
fill_map({Row, Col}, [{right, Length, _}|Input], AccMap) ->
    %io:fwrite("XXX ~p ~p ~p~n", [{Row, Col}, {right, Length}, AccMap]),
    NewAccMap1 = add_to_map_row(Row, Col+1, Length-1, $-, AccMap),
    NewAccMap2 = case Input of
        [{After, _, _}|_] ->
            Last = case After of
                up   -> $J;
                down -> $7
            end,
            add_to_map_row(Row, Col+Length, 1, Last, NewAccMap1);
        [] ->
            NewAccMap1
    end,
    fill_map({Row, Col+Length}, Input, NewAccMap2);

fill_map({Row, Col}, [{down, Length, _}|Input], AccMap) ->
    %io:fwrite("XXX ~p ~p ~p~n", [{Row, Col}, {down, Length}, AccMap]),
    NewAccMap1 = add_to_map_col(Col, Row+1, Length-1, $|, AccMap),
    NewAccMap2 = case Input of
        [{After, _, _}|_] ->
            Last = case After of
                right -> $L;
                left  -> $J
            end,
            add_to_map_col(Col, Row+Length, 1, Last, NewAccMap1);
        [] ->
            NewAccMap1
    end,
    fill_map({Row+Length, Col}, Input, NewAccMap2);
    
fill_map({Row, Col}, [{left, Length, _} |Input], AccMap) ->
    %io:fwrite("XXX ~p ~p ~p~n", [{Row, Col}, {left, Length}, AccMap]),
    NewAccMap1 = add_to_map_row(Row, Col-Length+1, Length-1, $-, AccMap),
    NewAccMap2 = case Input of
        [{After, _, _}|_] ->
            Last = case After of
                up   -> $L;
                down -> $F
            end,
            add_to_map_row(Row, Col-Length, 1, Last, NewAccMap1);
        [] ->
            NewAccMap1
    end,
    fill_map({Row, Col-Length}, Input, NewAccMap2);

fill_map({Row, Col}, [{up, Length, _}|Input], AccMap) ->
    %io:fwrite("XXX ~p ~p ~p~n", [{Row, Col}, {up, Length}, AccMap]),
    NewAccMap1 = add_to_map_col(Col, Row-Length+1, Length-1, $|, AccMap),
    NewAccMap2 = NewAccMap2 = case Input of
        [{After, _, _}|_] ->
            Last = case After of
                right -> $F;
                left  -> $7
            end,
            add_to_map_col(Col, Row-Length, 1, Last, NewAccMap1);
        [] ->
            NewAccMap1
    end,
    fill_map({Row-Length, Col}, Input, NewAccMap2).

fill_gaps([])                       -> [];
fill_gaps([{Start, End, Row}|Rows]) -> [{Start, End, fill_gaps_row(Row)}|fill_gaps(Rows)].
    
fill_gaps_row([_] = Row)                                                             -> Row;
fill_gaps_row([{_, To1, _} = Cols1, {From2, _, _} = Cols2|Row]) when From2 =:= To1+1 -> [Cols1 | fill_gaps_row([Cols2|Row])];
fill_gaps_row([{_, To1, _} = Cols1, {From2, _, _} = Cols2|Row])                      -> [Cols1, {To1+1, From2-1, $.} | fill_gaps_row([Cols2|Row])].
    
    
add_to_map_row(Row, ColS, Length, Tile, [{From, To, RowTiles} | Map]) when From =< Row, Row =< To ->
    Before = case From < Row of
        true  -> [{From, Row-1, RowTiles}];
        false -> []
    end,
    NewRowTiles = lists:sort([{ColS, ColS+Length-1, Tile}|RowTiles]),
    After = case Row < To of
        true  -> [{Row+1, To, RowTiles}];
        false -> []
    end,
    Before ++ [{Row, Row, NewRowTiles}] ++ After ++ Map;
    
add_to_map_row(Row, ColS, Length, Tile, [{From, To, RowTiles} | Map]) ->
    Result = add_to_map_row(Row, ColS, Length, Tile, Map),
    [{From, To, RowTiles}|Result].
    

add_to_map_col(_Col, _RowS, 0, _Tile, Map) -> Map;

add_to_map_col(Col, RowS, Length, Tile, [{From, To, RowTiles} | Map]) when From =< RowS, RowS =< To ->
    Before = case From < RowS of
        true  -> [{From, RowS-1, RowTiles}];
        false -> []
    end,
    NewRowTiles = lists:sort([{Col, Col, Tile}|RowTiles]),
    RowE = RowS + Length - 1,
    After = case RowE < To of
        true ->
            [{RowS, RowE, NewRowTiles}, {RowE+1, To, RowTiles} | Map];
        false ->
            Result = add_to_map_col(Col, To+1, Length-To+RowS-1, Tile, Map),
            [{RowS, To, NewRowTiles}|Result]
    end,
    Before ++ After;
    
add_to_map_col(Col, RowS, Length, Tile, [{From, To, RowTiles} | Map]) ->
    Result = add_to_map_col(Col, RowS, Length, Tile, Map),
    [{From, To, RowTiles}|Result].



count_inside(Map) ->
    lists:foldl(fun({RowS, RowE, RowTiles}, AccCount) ->
        {out, undefined, Count} = lists:foldl(fun({ColS, ColE, Tile}, {InOut, Start, AccC}) ->
            case {Tile, ColS, ColE, InOut, Start} of
                {$., _, _, in,    undefined} -> {in,                   undefined, AccC+ColE-ColS+1};
                {$., _, _, out,   undefined} -> {out,                  undefined, AccC};
                {$|, C, C, InOut, undefined} -> {invert_in_out(InOut), undefined, AccC+1};
                {$L, C, C, InOut, undefined} -> {InOut,                $L,        AccC+1};
                {$F, C, C, InOut, undefined} -> {InOut,                $F,        AccC+1};
                {$-, _, _, InOut, Start    } -> {InOut,                Start,     AccC+ColE-ColS+1};
                {$J, C, C, InOut, $F       } -> {invert_in_out(InOut), undefined, AccC+1};
                {$J, C, C, InOut, $L       } -> {InOut,                undefined, AccC+1};
                {$7, C, C, InOut, $F       } -> {InOut,                undefined, AccC+1};
                {$7, C, C, InOut, $L       } -> {invert_in_out(InOut), undefined, AccC+1}
            end
        end, {out, undefined, 0}, RowTiles),
        AccCount + (Count * (RowE - RowS + 1))
    end, 0, Map).

    
invert_in_out(in ) -> out;
invert_in_out(out) -> in.
    

trim_ending_newline(Str) ->
    string:sub_string(Str, 1, string:len(Str) - 1).

