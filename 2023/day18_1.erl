-module(day18_1).
-export([solve/1]).

% Privertė pamąstyti. Perskaitęs užduotį pamaniau - neįmanoma! Tada pradėjau galvoti, sumąsčiau algoritmą, bet jis buvo
% per daug paprastas ir neteisingas. Pamąsčiau dar, sumąsčiąu gerą algoritmą, bet jau neturėjau laiko toliau sėdėti.
% Tai buvo maždaug pusiaukelė. Po pertraukos vėl prisėdau, suprogramavau ir viskas veikė. Buvau trečias savo leader boarde
% tą išsprendęs (https://adventofcode.com/2023/leaderboard/private/view/438010?order=local_score).

solve(FileName) ->
    Input = get_input(FileName),
    {Size, Start} = get_size_and_start(Input),
    %io:fwrite("XXX ~p ~p~n", [Size, Start]),
    Map = get_map(Size, Start, Input),
    %io:fwrite("XXX MAP ~p~n", [Map]),
    %print_map(Map),
    Result = count_inside(Map, Size),
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
            [DirectionStr, LengthStr, Colour] = string:split(InputLine, " ", all),
            Direction = case DirectionStr of
                "R" -> right;
                "D" -> down;
                "L" -> left;
                "U" -> up
            end,
            Length = erlang:list_to_integer(LengthStr),
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


get_map({Rows, Cols}, Start, Input) ->
    InitMap = maps:from_list([ {N, #{}} || N <- lists:seq(1, Rows) ]),
    [First | _] = Input,
    Input2 = Input ++ [First],
    {Map, Start} = fill_map(Start, Input2, InitMap),
    %io:fwrite("XXX FILLED ~p~n", [Map]),
    lists:foldl(fun(Row, AccMap) ->
        lists:foldl(fun(Col, AccM) ->
            case get_cell({Row, Col}, AccM) of
                false -> fill_cell({Row, Col}, $., AccM);
                _     -> AccM
            end
        end, AccMap, lists:seq(1, Cols))
    end, Map, lists:seq(1, Rows)).

    
fill_map(Cell, [_], AccMap) -> {AccMap, Cell};
    
fill_map({Row, Col}, [{right, Length, _}|Input], AccMap) ->
    %io:fwrite("XXX ~p ~p ~p~n", [{Row, Col}, {right, Length}, AccMap]),
    RightCells = [ {Row, C} || C <- lists:seq(Col+1, Col+Length-1) ],
    NewAccMap1 = fill_cells(RightCells, $-, AccMap),
    NewAccMap2 = case Input of
        [{After, _, _}|_] ->
            Last = case After of
                up   -> $J;
                down -> $7
            end,
            fill_cell({Row, Col+Length}, Last, NewAccMap1);
        [] ->
            NewAccMap1
    end,
    fill_map({Row, Col+Length}, Input, NewAccMap2);

fill_map({Row, Col}, [{down, Length, _}|Input], AccMap) ->
    %io:fwrite("XXX ~p ~p ~p~n", [{Row, Col}, {down, Length}, AccMap]),
    DownCells = [ {R, Col} || R <- lists:seq(Row+1, Row+Length-1) ],
    NewAccMap1 = fill_cells(DownCells, $|, AccMap),
    NewAccMap2 = NewAccMap2 = case Input of
        [{After, _, _}|_] ->
            Last = case After of
                right -> $L;
                left  -> $J
            end,
            fill_cell({Row+Length, Col}, Last, NewAccMap1);
        [] ->
            NewAccMap1
    end,
    fill_map({Row+Length, Col}, Input, NewAccMap2);
    
fill_map({Row, Col}, [{left, Length, _} |Input], AccMap) ->
    %io:fwrite("XXX ~p ~p ~p~n", [{Row, Col}, {left, Length}, AccMap]),
    LeftCells = [ {Row, C} || C <- lists:seq(Col-Length+1, Col-1) ],
    NewAccMap1 = fill_cells(LeftCells, $-, AccMap),
    NewAccMap2 = case Input of
        [{After, _, _}|_] ->
            Last = case After of
                up   -> $L;
                down -> $F
            end,
            fill_cell({Row, Col-Length}, Last, NewAccMap1);
        [] ->
            NewAccMap1
    end,
    fill_map({Row, Col-Length}, Input, NewAccMap2);

fill_map({Row, Col}, [{up, Length, _}|Input], AccMap) ->
    %io:fwrite("XXX ~p ~p ~p~n", [{Row, Col}, {up, Length}, AccMap]),
    UpCells = [ {R, Col} || R <- lists:seq(Row-Length+1, Row-1) ],
    NewAccMap1 = fill_cells(UpCells, $|, AccMap),
    NewAccMap2 = NewAccMap2 = case Input of
        [{After, _, _}|_] ->
            Last = case After of
                right -> $F;
                left  -> $7
            end,
            fill_cell({Row-Length, Col}, Last, NewAccMap1);
        [] ->
            NewAccMap1
    end,
    fill_map({Row-Length, Col}, Input, NewAccMap2).


fill_cells([], _, AccMap) -> AccMap;
fill_cells([Cell|Input], With, AccMap) ->
    NewAccMap = fill_cell(Cell, With, AccMap),
    fill_cells(Input, With, NewAccMap).
    
fill_cell({Row, Col}, With, AccMap) ->
    #{Row := RowMap} = AccMap,
    NewRowMap = RowMap#{Col => {{Row, Col}, With}},
    AccMap#{Row => NewRowMap}.
    
    
get_cell({Row, Col}, Map) ->
    case maps:find(Row, Map) of
        {ok, RowMap} ->
            case maps:find(Col, RowMap) of
                {ok, Cell} -> {ok, Cell};
                error      -> false
            end;
        error ->
            false
    end.
    
    
%print_map(Map) ->
%    MapList = lists:sort(maps:to_list(Map)),
%    lists:foreach(fun({_, LineMap}) ->
%        LineMapList = lists:sort(maps:to_list(LineMap)),
%        lists:foreach(fun({_, {_, Tile}}) -> io:fwrite("~s", [[Tile]]) end, LineMapList),
%        io:fwrite("~n")
%    end, MapList).
    

count_inside(Map, {Rows, Cols}) ->
    lists:foldl(fun(Row, AccCount) ->
        {out, undefined, Count} = lists:foldl(fun(Col, {InOut, Start, AccC}) ->
            {ok, {{Row, Col}, Type}} = get_cell({Row, Col}, Map),
            case {Type, InOut, Start} of
                {$., in,    undefined} -> {in,                   undefined, AccC+1};
                {$., out,   undefined} -> {out,                  undefined, AccC};
                {$|, InOut, undefined} -> {invert_in_out(InOut), undefined, AccC+1};
                {$L, InOut, undefined} -> {InOut,                $L,        AccC+1};
                {$F, InOut, undefined} -> {InOut,                $F,        AccC+1};
                {$-, InOut, Start    } -> {InOut,                Start,     AccC+1};
                {$J, InOut, $F       } -> {invert_in_out(InOut), undefined, AccC+1};
                {$J, InOut, $L       } -> {InOut,                undefined, AccC+1};
                {$7, InOut, $F       } -> {InOut,                undefined, AccC+1};
                {$7, InOut, $L       } -> {invert_in_out(InOut), undefined, AccC+1}
            end
        end, {out, undefined, AccCount}, lists:seq(1, Cols)),
        Count    
    end, 0, lists:seq(1, Rows)).

    
invert_in_out(in ) -> out;
invert_in_out(out) -> in.
    

trim_ending_newline(Str) ->
    string:sub_string(Str, 1, string:len(Str) - 1).

