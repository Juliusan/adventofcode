-module(day13_2).
-export([solve/1]).

% Nesudėtingas, bet painus. Susipainiojau pats, tada dar labiau susipainiojau. Galiausiai įstrigau,
% nes viską skaičiuodavo, rezultatą gaudavo (bet neteisingą), pavyzdžius praeidavo, bet negalėjau
% surasti, kurį (kuriuos) žemėlapį skaičiuoja blogai. Supratau, kad nepatikrinu to, kad simetrijos
% ašis turi būtinai būti kita. Puoliau greitai taisyti tai vienur, tai kitur, nieko nesigavo ir
% užstrigau mirtinai. Net Rimo paprašiau kad suskaičiuotų man kiekvieno žemėlapio naująją simetriją
% (jis už mane anksčiau baigė), bet vis dėl to neprireikė. Galiausiai kitą dieną atsisėdau, dar kartą
% įdėmiai perskaičiau sąlygą, pasitikrinau, kad visi pradiniai žemėlapiai turi tik vieną simetrijos
% ašį, tada tvarkingai perrašiau kodą (na, sąlyginai tvarkingai, nes laikrodis tai tiksėjo), padariau,
% kad nulūžtų, jeigu negauna gero atsakymo, išnagrinėjau (padedant LibreOffice Calc) kelis pavyzdžius,
% su kuriais programa lūždavo, ištaisiau klaidas ir gavau gerą atsakymą. Valio! Pasirodo, problema
% buvo ta, kad jeigu simetrijos ašis būdavo tokio pat tipo (vertikali/horizontali), bet toliau nuo
% priekio, negu originalioji, tai jos tiesiog nerasdavo. Na iš tiesų ne taip sunku buvo.

solve(FileName) ->
    Maps = get_maps(FileName),
    Result = count_result_maps(Maps),
    Result.
    
    
get_maps(FileName) ->
    {ok, File} = file:open(FileName, [read]),
    Maps = get_maps(File, []),
    ok = file:close(File),
    Maps.
    
get_maps(File, AccMaps) ->
    {NewMap, IsEOF} = get_map(File, []),
    NewAccMaps = [lists:reverse(NewMap) | AccMaps],
    case IsEOF of
        true  -> NewAccMaps;
        false -> get_maps(File, NewAccMaps)
    end.


get_map(File, AccMap) ->
    case file:read_line(File) of
        eof ->
            {AccMap, true};
        {ok, "\n"} ->
            {AccMap, false};
        {ok, Line} ->
            %io:fwrite("XXX ~p: ", [Line]),
            MapLine = trim_ending_newline(Line),
            %io:fwrite("~p~n", [MapNode]),
            get_map(File, [MapLine | AccMap])
    end.


%count_result_maps([]) -> ok;
    
%count_result_maps([Map|Maps]) ->
%    Hor = count_single(Map),
%    MapT = transpose(Map),
%    Ver = count_single(MapT),
%    Res = case {Hor, Ver} of
%        {[] , [] } -> "FALSE";
%        {[] , [_]} -> "OK   ";
%        {[] , _  } -> "LIST2";
%        {[_], [] } -> "OK   ";
%        {_  , [] } -> "LIST1";
%        {_  , _  } -> "BOTH!"
%    end,
%    io:fwrite("XXX ~s: ~p ~p~n", [Res, Hor, Ver]),
%    count_result_maps(Maps).
    

count_result_maps(Maps) ->
    count_result_maps(Maps, 0).
    
count_result_maps([], AccResult) ->
    AccResult;
    
count_result_maps([Map | Maps], AccResult) ->
    %io:fwrite("VVVVVVVVVVVVVVVVVV~n"),
    %print_map(Map),
    %io:fwrite("AAAAAAAAAAAAAAAAAA~n"),
    Result = case count_result(Map) of
        {hor, Count} -> 100*Count;
        {ver, Count} -> Count
    end,
    count_result_maps(Maps, Result + AccResult).
    
    
count_result(Map) ->
    OrigCount = count_single(Map, undefined),
    %io:fwrite("XXX ORIG ~p~n", [OrigCount]),
    case count_result_with_fix(Map, 0, Map, OrigCount, false) of
        false ->
            %io:fwrite("XXX HORIZONTAL FALSE~n"),
            MapT = transpose(Map),
            count_result_with_fix(MapT, 0, MapT, OrigCount, true);
        Count ->
            %io:fwrite("XXX HORIZONTAL ~p~n", [Count]),
            Count
    end.

    
count_result_with_fix([_], _Index, _AllMap, _OrigCount, false) ->
    false;

count_result_with_fix([Line1, Second | Map], Index, AllMap, OrigCount, IsTransposed) ->
    case count_result_with_fix(Line1, [Second|Map], Index, Index+1, AllMap, OrigCount, IsTransposed) of
        false -> count_result_with_fix([Second|Map], Index+1, AllMap, OrigCount, IsTransposed);
        Count -> Count
    end.

count_result_with_fix(_Line1, [], _Index1, _Index2, _AllMap, _OrigCount, _IsTransposed) ->
%    io:fwrite("XXX ~p ~p EMPTY~n", [_Index1, _Index2]),
    false;
    
count_result_with_fix(Line1, [Line2 | Map], Index1, Index2, AllMap, OrigCount, IsTransposed) ->
%    io:fwrite("XXX ~p ~p~n", [Index1, Index2]),
    case fix_line(Line1, Line2) of
        false ->
            count_result_with_fix(Line1, Map, Index1, Index2+1, AllMap, OrigCount, IsTransposed); 
        {NewLine1, NewLine2} ->
            %io:fwrite("XXX Fix ~p ~p (~p) -> ~p ~p~n", [Index1, Index2, IsTransposed, NewLine1, NewLine2]),
            case replace_and_count(NewLine1, Index1, AllMap, OrigCount, IsTransposed) of
                false -> 
                    case replace_and_count(NewLine2, Index2, AllMap, OrigCount, IsTransposed) of
                        false -> count_result_with_fix(Line1, Map, Index1, Index2+1, AllMap, OrigCount, IsTransposed);
                        Count -> Count
                    end;
                Count ->
                    Count
            end
    end.
            

replace_and_count(NewLine, Index, Map, OrigCount, IsTransposed) ->
    %io:fwrite("XXXR ~p ~p ~p~n", [NewLine, Index, IsTransposed]),
    {First, [_|Last]} = split_by_count(Index, Map),
    NewMap  = First ++ [NewLine] ++ Last,
    %print_map(Map),
    %io:fwrite("..................~n"),
    %print_map(NewMap),
    %io:fwrite("------------------~n"),
    MapN = case IsTransposed of
        false -> NewMap;
        true  -> transpose(NewMap)
    end,
    count_single(MapN, OrigCount).
    
    
count_single(Map, OrigCount) ->    
    {OrigCountH, OrigCountV} = case OrigCount of
        undefined -> {undefined, undefined};
        {hor, C}  -> {C,         undefined};
        {ver, C}  -> {undefined, C        }
    end,
    %io:fwrite("XXXR OC=~p ~p~n", [OrigCount, find_symetry(Map, OrigCountH)]),
    case find_symetry(Map, OrigCountH) of
        false ->
            MapT = transpose(Map),
            %io:fwrite("XXXR OC=~p ~p~n", [OrigCount, find_symetry(MapT, OrigCountV)]),
            case find_symetry(MapT, OrigCountV) of
                false -> false;
                Count -> {ver, Count}
            end;
        Count ->
            {hor, Count}
    end.


find_symetry(Map, NotLine) ->
    MapR = lists:reverse(Map),
    Length = erlang:length(Map),
    [Line1 | OtherMap] = Map,
    find_symetry(OtherMap, Line1, 1, {Map, MapR, Length}, NotLine).

    
find_symetry([], _, _, _, _) ->
    false;

find_symetry([Line1|Map], _Line0, Index, MapData, Index) ->
    find_symetry(Map, Line1, Index+1, MapData, Index);
    
find_symetry([Line1|Map], Line0, Index, MapData, NotIndex) ->
    case lines_equal(Line0, Line1) of
        true ->
            %io:fwrite("XXX EQUAL ~p ~p~n", [Index, is_symetry(Index, MapData)]),
            case is_symetry(Index, MapData) of
                true  -> Index;
                false -> find_symetry(Map, Line1, Index+1, MapData, NotIndex)
            end;
        false ->
            find_symetry(Map, Line1, Index+1, MapData, NotIndex)
    end.
    
%find_symetry([], _, _, _, Acc) ->
%    Acc;
    
%find_symetry([Line1|Map], Line0, Index, MapData, Acc) ->
%    case lines_equal(Line0, Line1) of
%        true ->
%            %io:fwrite("XXX EQUAL ~p ~p~n", [Index, is_symetry(Index, MapData)]),
%            NewAcc = case is_symetry(Index, MapData) of
%                true  -> [Index | Acc];
%                false -> Acc
%            end,
%            find_symetry(Map, Line1, Index+1, MapData, NewAcc);
%        false ->
%            find_symetry(Map, Line1, Index+1, MapData, Acc)
%    end.
    
    
lines_equal(A, B) -> A=:=B.


fix_line(Line1, Line2) ->
    fix_line(Line1, Line2, {"", "", 0}).

fix_line([], [], {_AccLine1, _AccLine2, 0}) -> false;

fix_line([], [], {AccLine1, AccLine2, 1}) -> {lists:reverse(AccLine1), lists:reverse(AccLine2)};

fix_line([A|Line1], [A|Line2], {AccLine1, AccLine2, Count}) ->
    fix_line(Line1, Line2, {[A|AccLine1], [A|AccLine2], Count});

fix_line([A|Line1], [B|Line2], {AccLine1, AccLine2, 0}) when A =/= B ->
    fix_line(Line1, Line2, {[B|AccLine1], [A|AccLine2], 1});
    
fix_line([A|_Line1], [B|_Line2], {_AccLine, _AccLine2, 1}) when A =/= B ->
    false.


is_symetry(Index, {Map, MapR, Length}) ->
    LinesBelow = Length - Index,
    LinesAbove = Index,
    {Map1, Map2, Count} = case LinesAbove > LinesBelow of
        true  -> {skip_first(LinesAbove-LinesBelow, Map), MapR,                                      LinesBelow};
        false -> {                                  Map , skip_first(LinesBelow - LinesAbove, MapR), LinesAbove}
    end,
    compare(Map1, Map2, Count).
    
    
compare(_, _, 0) -> true;    

compare([Line1|Map1], [Line2|Map2], Count) ->
    case lines_equal(Line1, Line2) of
        true  -> compare(Map1, Map2, Count-1);
        false -> false
    end.


%print_map([]) -> ok;
%print_map([S|OtherRows]) ->
%    io:fwrite("Map ~p~n", [S]),
%    print_map(OtherRows).


transpose([[]|_]) -> [];
transpose(M) ->
  [lists:map(fun hd/1, M) | transpose(lists:map(fun tl/1, M))].
    

skip_first(Count, List) ->
    {_, Remaining} = split_by_count(Count, List),
    Remaining.


split_by_count(Count, List) ->
    split_by_count(Count, List, []).

split_by_count(0,          Remaining,  First) -> {lists:reverse(First), Remaining};
split_by_count(Count, [E | Remaining], First) -> split_by_count(Count-1, Remaining, [E | First]).


trim_ending_newline(Str) ->
    string:sub_string(Str, 1, string:len(Str) - 1).

