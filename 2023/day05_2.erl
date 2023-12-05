-module(day05_2).
-export([solve/1]).

% apie 2h - iš pradžių galvojau, kad bus trivialu, padariau, praėjo testinį pavyzdį, o tada nulaužiau kompą darydamas realų.
% Out of memory - nes bandžiau kelis milijardus skaičių surašyti į vieną listą. Teko daryti ne taip atminčiai alkaną variantą.
% Na ir įklimpau. Bandžiau daryti labai gudrų algoritmą ir labai painiojausi... Na bent jau gera smegenų mankšta buvo.

solve(FileName) ->
    {ok, File} = file:open(FileName, [read]),
    Seeds = read_seeds(File),
    {ok, "\n"} = file:read_line(File),
    Maps = read_maps(File),
    ok = file:close(File),
    Result = find_result(Seeds, Maps),
    Result.
    
    
read_seeds(File) ->
    {ok, FirstLine} = file:read_line(File),
    "seeds: " ++ SeedsStr = FirstLine,
    SeedsStrNoNewLine = trim_ending_newline(SeedsStr), 
    SeedRanges = numbers_str_to_list(SeedsStrNoNewLine),
    AllSeedsFun = fun 
        AllSeedsFun([], AccSeeds) ->
            AccSeeds;
        AllSeedsFun([SeedStart, Length | OtherRanges], AccSeeds) ->
            NewSeed = {SeedStart, SeedStart + Length - 1},
            AllSeedsFun(OtherRanges, [ NewSeed | AccSeeds ])
    end,
    AllSeedsFun(SeedRanges, []).


read_maps(File) ->
    ResultFun = fun ResultFun(AccMaps) ->
        case read_map(File) of
            {eof, Map} ->
                lists:reverse([Map | AccMaps]);
            {ok, Map} ->
                ResultFun([Map | AccMaps])
        end
    end,
    ResultFun([]).
    
    
read_map(File) ->
    {ok, MapNameLine} = file:read_line(File),
    [MapName, "map:\n"] = string:split(MapNameLine, " "),
    ResultFun = fun ResultFun(AccEntries) ->
        case file:read_line(File) of
            eof ->
                {eof, AccEntries};
            {ok, "\n"} ->
                {ok, AccEntries};
            {ok, EntryStr} ->
                EntryStrNoNewLine = trim_ending_newline(EntryStr), 
                [DestStart, SourceStart, Length] = numbers_str_to_list(EntryStrNoNewLine),
                ResultFun([{SourceStart, DestStart, Length} | AccEntries])
        end
    end,
    {Result, Entries} = ResultFun([]),
    {Result, {MapName, Entries}}.

find_result(Seeds, Maps) ->
    %io:fwrite("XXX ~p ~p~n", [Seeds, Maps]),
    lists:foldl(fun({SeedStart, SeedEnd}, AccMinLocation) ->
        %io:fwrite("XXX ~p-~p ~p...~n", [SeedStart, SeedEnd, AccMinLocation]),
        Locations = lists:foldl(fun({_MapName, Entries}, Ins) ->
            %io:fwrite("  XXX ~p: ~p...~n", [MapName, Ins]),
            %io:fread("", ""),
            {NewIns, Outs} = lists:foldl(fun({SourceStart, DestStart, Length}, {InsLeft, AccOuts}) ->
                %io:fwrite("    XXX ~p ~p ~p; ~p ~p...~n", [SourceStart, DestStart, Length, InsLeft, AccOuts]),
                %io:fread("", ""),
                SourceEnd = SourceStart + Length - 1,
                {NewInsLeft, NewAccOuts} = lists:foldl(fun(InInterval, {InsNotApplied, AccApplied}) ->
                    %io:fwrite("      XXX ~p; ~p ~p...~n", [InInterval, InsNotApplied, AccApplied]),
                    %io:fwrite("      XXX ~p: ~p ~p~n", [InInterval, intersect(InInterval, {SourceStart, SourceEnd}), substract(InInterval, {SourceStart, SourceEnd})]),
                    %io:fread("", ""),
                    Result = case intersect(InInterval, {SourceStart, SourceEnd}) of
                        undefined ->
                            {[[InInterval] | InsNotApplied], AccApplied};
                        {NewInStart, NewInEnd} ->
                            NewInNotApplied = substract(InInterval, {SourceStart, SourceEnd}),
                            AppliedStart = DestStart + NewInStart - SourceStart,
                            AppliedEnd   = DestStart + NewInEnd   - SourceStart,
                            {[NewInNotApplied | InsNotApplied], [{AppliedStart, AppliedEnd} | AccApplied]}
                    end,
                    %io:fwrite("      XXX ~p = ~p~n", [InInterval, Result]),
                    %io:fread("", ""),
                    Result
                end, {[], AccOuts}, InsLeft),
                %io:fwrite("    XXX ~p ~p ~p = ~p ~p~n", [SourceStart, DestStart, Length, lists:append(NewInsLeft), NewAccOuts]),
                %io:fread("", ""),
                {lists:append(NewInsLeft), NewAccOuts}
            end, {Ins, []}, Entries),
            %io:fwrite("  XXX ~p = ~p~n", [MapName, NewIns ++ Outs]),
            %io:fread("", ""),
            NewIns ++ Outs
        end, [{SeedStart, SeedEnd}], Maps),
        FinalLocation = lists:foldl(fun({LocationStart, _}, AccML) ->
            case {LocationStart, AccML} of
                {_, undefined}    -> LocationStart;
                {L, A} when L < A -> LocationStart;
                _                 -> AccML
            end
        end, AccMinLocation, Locations),
        %io:fwrite("XXX ~p-~p = ~p~n", [SeedStart, SeedEnd, FinalLocation]),
        FinalLocation
    end, undefined, Seeds).
    
    
intersect({S1, E1}, {S2, E2}) when S1 =< S2, E2 =< E1           -> {S2, E2};
intersect({S1, E1}, {S2, E2}) when S2 =< S1, E2 =< E1, S1 =< E2 -> {S1, E2};
intersect({S1, E1}, {S2, E2}) when S2 =< S1, E1 =< E2           -> {S1, E1};
intersect({S1, E1}, {S2, E2}) when S1 =< S2, E1 =< E2, S2 =< E1 -> {S2, E1};
intersect({_ , _ }, {_ , _ })                                   -> undefined.


substract({S1, E1}, {S2, E2}) when S1  < S2, E2  < E1            -> [{S1, S2-1},{E2+1, E1}];
substract({S1, E1}, {S2, E2}) when S2 =< S1, E2  < E1, S1 =< E2  -> [{E2+1, E1}];
substract({S1, E1}, {S2, E2}) when S2 =< S1, E1 =< E2            -> [];
substract({S1, E1}, {S2, E2}) when S1  < S2, E1 =< E2, S2 =< E1  -> [{S1, S2-1}];
substract({S1, E1}, {_ , _ })                                    -> [{S1, E1}].
    
    
trim_ending_newline(Str) ->
    string:sub_string(Str, 1, string:len(Str) - 1).
        
    
numbers_str_to_list(NumbersStr) ->
    NumbersStrList = string:split(NumbersStr, " ", all),
    lists:filtermap(fun
        ("")        -> false;
        (NumberStr) -> {true, erlang:list_to_integer(NumberStr)}
    end, NumbersStrList).
