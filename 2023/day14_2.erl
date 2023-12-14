-module(day14_2).
-export([solve/1]).

% Iš pradžių bandžiau realiai perinkti milijardą ciklų. Po to, kai vos pavyko perrinkti 1000, pagalvojau,
% kad galbūt nuo kažkurios vietos žemėlapis po ciklo neturėtų keistis. Kai tai nepasiteisino, pagalvojau,
% kad galbūt kažkur žemėlapiai, gauti po ciklo, ciklinasi ir tai buvo Eureko momentas. Bet tai tilpo gal
% į pusvalandį. Kaip ten bebūtų pavyzdys nepraėjo. Tada puoliau analizuoti ir ieškoti klaidos kode, gal
% ne taip ciklas veikia. Bet su pavyzdžiais sutampa, tai gal ciklų skaičius blogai suskaičiuotas. Tą
% tikrindamas praleidau nemažai laiko. Net fake pavyzdį ciklų skaičiavimui pasirašiau. O problema, pasirodo,
% buvo visai ne ten. Pasirodo spaudimą į šiaurę reikia skaičiuoti, kai į tą šiaurę niekas nespaudžia. Tai
% yra, kai lenta nepasukta į šiaurę. Aš pirmo uždavino įpročiu į šiaurę pasukdavau.

solve(FileName) ->
    Map = get_map(FileName),
    %print_map(Map),
    MapC = run_cycles(Map),
    %io:fwrite("VVVVVVVVVVVVVVVVVVV~n"),
    %print_map(MapC),
    Weight = weight(MapC),
    Weight.
    
    
get_map(FileName) ->
    {ok, File} = file:open(FileName, [read]),
    Map = get_map(File, []),
    ok = file:close(File),
    Map.
    
get_map(File, AccMap) ->
    case file:read_line(File) of
        eof ->
            lists:reverse(AccMap);
        {ok, Line} ->
            %io:fwrite("XXX ~p: ", [Line]),
            MapLine = trim_ending_newline(Line),
            %io:fwrite("~p~n", [MapNode]),
            get_map(File, [MapLine | AccMap])
    end.
    
    
run_cycles(Map) ->
    %run_cycles(Map, 3, []).
    run_cycles(Map, 1000000000, []).
    
    
%run_cycles(Map, 0, _) ->
%    Map;

run_cycles(Map, N, AccMaps) ->
    NewMap = run_cycle(Map),
    case index_of(NewMap, AccMaps) of
        false ->
            run_cycles(NewMap, N-1, [NewMap | AccMaps]);
        Index ->
            Remaining = N rem Index,
            {Cycles, _} = split_by_count(Index, AccMaps),
            lists:nth(Remaining, lists:reverse(Cycles))
    end.
    
    
%run_cycleN(N) when N < 6 -> N+1;
%run_cycleN(6) -> 3.

run_cycle(Map) ->
    % North
    MapNI = transpose(Map),
    MapNO = tilt(MapNI, false),
    % West
    MapWI = transpose(MapNO),
    MapWO = tilt(MapWI, false),
    % South
    MapSI = transpose(MapWO),
    MapSO = tilt(MapSI, true),
    % East
    MapEI = transpose(MapSO),
    MapEO = tilt(MapEI, true),
    %io:fwrite("-------------------~n"),
    %print_map(MapEO),
    MapEO.


tilt(Map, Reverse) ->
    lists:map(fun(Line) ->
        tilt_line(Line, Reverse)
    end, Map).
    
    
tilt_line(Line, Reverse) ->
    Stones = count_stones(Line, []),
    get_line(Stones, Reverse).
    
    
count_stones("",                      AccStones ) -> lists:reverse(AccStones);
count_stones([$O|Line],  [{Os,Dots} | AccStones]) -> count_stones(Line, [{Os+1, Dots}|AccStones]);
count_stones([$O|Line],               AccStones ) -> count_stones(Line, [{1,    0   }|AccStones]);
count_stones([$.|Line],  [{Os,Dots} | AccStones]) -> count_stones(Line, [{Os, Dots+1}|AccStones]);
count_stones([$.|Line],               AccStones ) -> count_stones(Line, [{0,  1     }|AccStones]);
count_stones([$#|Line],  [{Sharps}  | AccStones]) -> count_stones(Line, [{Sharps+1}  |AccStones]);
count_stones([$#|Line],               AccStones ) -> count_stones(Line, [{1       }  |AccStones]).

%count_weight([],               _,      AccSum) -> AccSum;
%count_weight([{Os,Dots}|Else], Weight, AccSum) -> count_weight(Else, Weight-Os-Dots, AccSum + (Os*(2*Weight-Os+1) div 2));
%count_weight([{Sharps} |Else], Weight, AccSum) -> count_weight(Else, Weight-Sharps,  AccSum).

count_weight_line("",        _,      AccSum) -> AccSum;
count_weight_line([$O|Line], Weight, AccSum) -> count_weight_line(Line, Weight-1, AccSum+Weight);
count_weight_line([_ |Line], Weight, AccSum) -> count_weight_line(Line, Weight-1, AccSum       ).

get_line([],               _    )               -> "";
get_line([{Os,Dots}|Else], false) -> lists:duplicate(Os, $O) ++ lists:duplicate(Dots, $.) ++ get_line(Else, false);
get_line([{Os,Dots}|Else], true ) -> lists:duplicate(Dots, $.) ++ lists:duplicate(Os, $O) ++ get_line(Else, true );
get_line([{Sharps} |Else], Rev  ) -> lists:duplicate(Sharps, $#) ++ get_line(Else, Rev).

weight(Map) ->
    MapT = transpose(Map),
    lists:foldl(fun(Line, AccSum) ->
        Length = erlang:length(Line),
        %Stones = count_stones(Line, []),
        %Sum = count_weight(Stones, Length, 0),
        Sum = count_weight_line(Line, Length, 0),
        %io:fwrite("XXX ~p -> ~p~n", [Line, Sum]),
        Sum + AccSum
    end, 0, MapT).


%print_map([]) -> ok;
%print_map([S|OtherRows]) ->
%    io:fwrite("Map ~p~n", [S]),
%    print_map(OtherRows).


transpose([[]|_]) -> [];
transpose(M) ->
  [lists:map(fun hd/1, M) | transpose(lists:map(fun tl/1, M))].
  

index_of(Elem, List) -> index_of(Elem, List, 1).
index_of(_,    [],       _    ) -> false;
index_of(Elem, [Elem|_], Index) -> Index;
index_of(Elem, [_|List], Index) -> index_of(Elem, List, Index+1).


split_by_count(Count, List) -> split_by_count(Count, List, []).
split_by_count(0,          Remaining,  First) -> {lists:reverse(First), Remaining};
split_by_count(Count, [E | Remaining], First) -> split_by_count(Count-1, Remaining, [E | First]).


trim_ending_newline(Str) ->
    string:sub_string(Str, 1, string:len(Str) - 1).

