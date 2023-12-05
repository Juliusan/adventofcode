-module(day05_1).
-export([solve/1]).

% 0:37:40 - daug programavimo; nebuvo sunku.

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
    numbers_str_to_list(SeedsStrNoNewLine).


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
    lists:foldl(fun(Seed, AccMinLocation) ->
        Location = lists:foldl(fun({_MapName, Entries}, AccValue) ->
            MapFun = fun 
                MapFun([]) ->
                    AccValue;
                MapFun([{SourceStart, DestStart, Length} | OtherEntries]) ->
                    SourceEnd = SourceStart + Length - 1,
                    case SourceStart =< AccValue andalso AccValue =< SourceEnd of
                        true  -> DestStart + AccValue - SourceStart;
                        false -> MapFun(OtherEntries)
                    end
            end,
            MapFun(Entries)
        end, Seed, Maps),
        case {AccMinLocation, Location} of
            {undefined, _}    -> Location;
            {A, L} when L < A -> Location;
            _                 -> AccMinLocation
        end
    end, undefined, Seeds).
    
    
trim_ending_newline(Str) ->
    string:sub_string(Str, 1, string:len(Str) - 1).
        
    
numbers_str_to_list(NumbersStr) ->
    NumbersStrList = string:split(NumbersStr, " ", all),
    lists:filtermap(fun
        ("")        -> false;
        (NumberStr) -> {true, erlang:list_to_integer(NumberStr)}
    end, NumbersStrList).
