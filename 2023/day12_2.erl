-module(day12_2).
-export([solve/1]).
-export([get_splits/2]).

% Pataisyti pirmą programą neužtruko, bet ir vėl užsiroviau ant efektyvumo. O tada prasidėjo: 
% visų pirma, viską sudėjau į vieną ciklą (kuriame, žinoma, dar yra ciklų), nes daryti ciklus
% nuosekliai užtrunka daug laiko - neįmanoma atmesti variantų iš anksto. O tada optimizacija
% ant optimizacijos ir kodas tapo neskaitomas. Na bet bent jau veikia. Atrodo...

solve(FileName) ->
    {ok, File} = file:open(FileName, [read]),
    GetSpringsFun = fun GetSpringsFun(AccSprings) ->
        case file:read_line(File) of
            eof ->
                AccSprings;
            {ok, Line} ->
                %io:fwrite("XXX ~p: ", [Line]),
                SpringLine = get_springs(Line),
                %io:fwrite("~p~n", [MapNode]),
                GetSpringsFun([SpringLine | AccSprings])
        end
    end,
    Springs = GetSpringsFun([]),
    ok = file:close(File),
    CodedSprings = encode_springs(Springs),
    %print_springs(CodedSprings),
    Options = count_options(CodedSprings),
    Options.
    
    
get_springs(Line) ->
    LineNoNewLine = trim_ending_newline(Line),
    [SpringsAll, CodeStr] = string:split(LineNoNewLine, " "),
    SpringsMul = SpringsAll ++ "?" ++ SpringsAll ++ "?" ++ SpringsAll ++ "?" ++ SpringsAll ++ "?" ++ SpringsAll,
    %SpringsMul = SpringsAll,
    Springs = string:split(SpringsMul, ".", all),
    SpringsNoEmpty = lists:filter(fun("") -> false; (_) -> true end, Springs),
    Code = numbers_str_to_list(CodeStr),
    CodeMul = Code ++ Code ++ Code ++ Code ++ Code,
    %CodeMul = Code,
    {SpringsNoEmpty, CodeMul}.
    
    
encode_springs(Springs) ->
    lists:map(fun(SpringsLine) ->
        {SpringsMap, Code} = SpringsLine,
        NewSpringsMap = lists:map(fun(SpringsCluster) ->
            [First | OSC] = SpringsCluster,
            AccIn = case First of
                $# -> [1];
                $? -> [-1]
            end,
            lists:reverse(lists:foldl(fun
                ($#, [C | OC]) when C > 0 -> [C+1 | OC];
                ($#, [C | OC]) when C < 0 -> [1, C | OC];
                ($?, [C | OC]) when C > 0 -> [-1, C | OC];
                ($?, [C | OC]) when C < 0 -> [C-1 | OC]
            end, AccIn, OSC))
        end, SpringsMap),
        {NewSpringsMap, Code}
    end, Springs).

    
count_options(Springs) -> 
    lists:foldl(fun({Map, Code}, AccSum) ->
        %io:fwrite("XXX ~p <- ~p: ", [Map, Code]),
        Splits = get_splits(Map, Code),
        %io:fwrite("~p~n", [Splits]),
        Splits + AccSum
    end, 0, Springs).
    
    
%count_options(Map, Code) -> 1.
count_options([], []) ->
%    io:fwrite("    XXX1 ~p ~p -> 1~n", [[], []]),
    1;

count_options([FMap], []) when FMap < 0 ->
%    io:fwrite("    XXX2 ~p ~p -> 1~n", [[FMap], []]),
    1;

count_options(_Map, []) ->
%    io:fwrite("    XXX8 ~p ~p -> 0~n", [_Map, []]),
    0;

count_options(Map, Code) ->
    MapSum = count_length_map_elem(Map),
    CodeSum = count_length_code(Code),          
    case MapSum >= CodeSum of
        true  ->
            count_options2(Map, Code);
        false ->
%            io:fwrite("    XXX3 ~p ~p -> 0~n", [[], Code]),
            0
    end.

count_options2([FMap | _OMap], [FCode | _OCode]) when FMap > 0,  FMap > FCode ->
%    io:fwrite("    XXX4 ~p ~p -> 0~n", [[FMap | _OMap], [FCode | _OCode]]),
    0;
     
count_options2([FMap | OMap], [FCode | OCode]) when FMap > 0,  FMap =:= FCode ->
%    io:fwrite("    XXX5 ~p ~p...~n ", [[FMap | OMap], [FCode | OCode]]),
    NewOMap = case OMap of
        []              -> [];
        [-1    | OOMap] -> OOMap;
        [FOMap | OOMap] -> [FOMap+1 | OOMap]
    end,
    Res = count_options(NewOMap, OCode),
%    io:fwrite("    XXX5 ~p ~p -> ~p~n", [[FMap | OMap], [FCode | OCode], Res]),
    Res;
     
count_options2([FMap | OMap], [FCode | OCode]) when FMap > 0,  FMap < FCode ->
%    io:fwrite("    XXX6 ~p ~p...~n", [[FMap | OMap], [FCode | OCode]]),
    Res = case count_options_consecutive(OMap, FCode-FMap) of
        false   -> 0;
        NewOMap -> count_options(NewOMap, OCode)
    end,
%    io:fwrite("    XXX6 ~p ~p -> ~p~n", [[FMap | OMap], [FCode | OCode], Res]),
    Res;

count_options2([FMap], []) when FMap < 0 ->
    1;

count_options2([FMap], Code) when FMap =< 0 ->
%    io:fwrite("    XXX9 ~p ~p...~n", [[FMap], Code]),
    CodeCount = erlang:length(Code),
    CodeSum = lists:sum(Code),
    Positions = -FMap - CodeSum + 1,
%    io:fwrite("    XXX9 ~p ~p: ~p ~p ~p~n", [[FMap], Code, Positions, Positions-CodeCount, CodeCount]),
    Res = case {Positions > 0, Positions-CodeCount >= 0} of
        {true, true} -> (fact(Positions) div fact(Positions-CodeCount)) div fact(CodeCount);
        _            -> 0
    end,
%    io:fwrite("    XXX9 ~p ~p -> ~p~n", [[FMap], Code, Res]),
    Res;
    
count_options2([FMap, SMap | OMap], [FCode | OCode]) when FMap < 0, SMap > 0, -FMap > FCode ->
%    io:fwrite("    XXXA ~p ~p~n", [[FMap, SMap | OMap], [FCode | OCode]]),
    CountOptionsFun = fun 
        CountOptionsFun(Count, AccSum) when Count >= length([FCode | OCode]) ->
            AccSum;
        CountOptionsFun(Count, AccSum) ->
            {First, [Middle | Last]} = split_by_count([FCode | OCode], Count),
%            io:fwrite("    XXXA-1 ~p ~p: ~p ~p ~p ~p~n", [[FMap, SMap | OMap], [FCode | OCode], First, Middle, [Middle | Last], SMap]),
%            io:fread("", ""),
            case Middle < SMap of
                true ->
                    CountOptionsFun(Count+1, AccSum);
                false ->
                    ShiftFun = fun ShiftFun(Missing, PrevOptsFirst, AS) ->
                        {Opts, OF} = case -FMap =< Missing of
                            true ->
%                                io:fwrite("    XXXA-2 ~p ~p: ~p ~p~n", [[FMap, SMap | OMap], [FCode | OCode], Missing, 0]),
%                                io:fread("", ""),
                                {0, 0};
                            false ->
                                case count_options2([FMap+Missing], First) of
                                    0 ->
                                        {0, 0};
                                    OptsFirst ->
                                    
                                        case -Missing+1 =< FMap of
                                            true  -> io:fwrite("    XXXA-3 ~p ~p: EUREKA~n", [[FMap, SMap | OMap], [FCode | OCode]]);
                                            false -> ok
                                        end,
                                        NewMap = case Missing of
                                            1 -> [SMap | OMap];
                                            _ -> [-Missing+1, SMap | OMap]
                                        end,
                                        OptsLast  = count_options(NewMap, [Middle | Last]),
         %                               io:fwrite("    XXXA-3 ~p ~p: ~p ~p ~p ~p ~p~n", [[FMap, SMap | OMap], [FCode | OCode], Missing, OptsFirst, OptsLast, NewMap, [Middle | Last]]),
         %                               io:fread("", ""),
                                        {(OptsFirst-PrevOptsFirst)*OptsLast, OptsFirst}
                                end
                        end,
                        case Missing > 1 of
                            true  -> ShiftFun(Missing-1, OF, Opts + AS);
                            false -> CountOptionsFun(Count+1, AS + Opts + AccSum)
                        end
                    end,
                    ShiftFun(Middle - SMap + 1, 0, 0)
            end
    end,
    OOptions = count_options([-FCode, SMap | OMap], [FCode | OCode]),
    Res = CountOptionsFun(1, OOptions),
  %  io:fwrite("    XXXA ~p ~p -> ~p~n", [[FMap, SMap | OMap], [FCode | OCode], Res]),
    Res;

count_options2([FMap, SMap | OMap], [FCode | OCode]) when FMap < 0, SMap > 0, -FMap =< FCode ->
%    io:fwrite("    XXX7 ~p ~p...~n", [[FMap, SMap | OMap], [FCode | OCode]]),
    MapMinus1 = case FMap of
        -1 -> [SMap | OMap];
        _  -> [FMap+1, SMap | OMap]
    end,
    OptionsDot = count_options(MapMinus1, [FCode | OCode]),
    OptionsSpring = case count_options_consecutive(MapMinus1, FCode-1) of
            false   -> 0;
            NewOMap -> count_options(NewOMap, OCode)
    end,
    Res = OptionsDot + OptionsSpring,
%    io:fwrite("    XXX7 ~p ~p -> ~p~n", [[FMap, SMap | OMap], [FCode | OCode], Res]),
    Res.


count_options_consecutive([],                  0     )                                            -> [];
count_options_consecutive([],                  _Count)                                            -> false;
count_options_consecutive([FMap       | _OMap], Count) when FMap > 0,            FMap  >  Count   -> false;
count_options_consecutive([FMap, -1   |  OMap], Count) when FMap > 0,            FMap =:= Count   -> OMap;
count_options_consecutive([FMap              ], Count) when FMap > 0,            FMap =:= Count   -> [];
count_options_consecutive([FMap, SMap |  OMap], Count) when FMap > 0,  SMap < 0, FMap =:= Count   -> [SMap + 1 | OMap];
count_options_consecutive([FMap       |  OMap], Count) when FMap > 0,            FMap  <  Count   -> count_options_consecutive(OMap, Count-FMap);
count_options_consecutive([FMap       |  OMap], Count) when FMap < 0,           -FMap  >  Count+1 -> [FMap + Count + 1 | OMap];
count_options_consecutive([FMap       |  OMap], Count) when FMap < 0,           -FMap =:= Count+1 -> OMap;
count_options_consecutive([FMap              ], Count) when FMap < 0,           -FMap =:= Count   -> [];
count_options_consecutive([FMap       | _OMap], Count) when FMap < 0,           -FMap =:= Count   -> false;
count_options_consecutive([FMap       |  OMap], Count) when FMap < 0,           -FMap  <  Count   -> count_options_consecutive(OMap, Count+FMap).

 
get_splits([], []) -> 1;
get_splits([], _) -> 0;
get_splits([FMap|OMap], Codes) ->
%    io:fwrite("XXX1 ~p ~p~n", [[FMap|OMap], Codes]),
    FMapLength = count_length_map_elem(FMap),
    GetSplitsFun = fun GetSplitsFun(First, AccSplits) ->
%        io:fwrite("  XXX2 ~p ~p ~p~n", [First, AccSplits, FMapLength]),
        case First =< erlang:length(Codes) of
            true ->
                {FCodes, OCodes} = split_by_count(Codes, First),
                FirstCodesLength = count_length_code(FCodes),
%                io:fwrite("    XXX3 ~p ~p ~p~n", [FCodes, OCodes, FirstCodesLength]),
                case FMapLength >= FirstCodesLength of
                    true ->
                        OMapLength = count_length_map(OMap),
                        OCodesLength = erlang:length(OCodes),
%                        io:fwrite("    XXX4 ~p ~p~n", [OMapLength, OCodesLength]),
                        case OMapLength >= OCodesLength of
                            true ->
                                case count_options(FMap, FCodes) of
                                    0 ->
                                        GetSplitsFun(First + 1, AccSplits);
                                    Count ->
                                        NewOthersSplits = get_splits(OMap, OCodes),
                                        NewSplits = Count * NewOthersSplits,
                                        NewAccSplits = NewSplits + AccSplits,
%                                        NewSplits = lists:map(fun(S) -> [FCodes | S] end, NewOthersSplits),
%                                       io:fwrite("    XXX5 ~p ~p -> ~p ~p~n", [[FMap|OMap], Codes, NewSplits, NewOthersSplits]),
%                                        NewAccSplits =  NewSplits ++ AccSplits,
                                        GetSplitsFun(First + 1, NewAccSplits)
                                end;
                            false ->
                                GetSplitsFun(First + 1, AccSplits)
                        end;
                    false ->
                        AccSplits
                end;
            false ->
                AccSplits
        end
    end,
    GetSplitsFun(0, 0).


count_length_map_elem(MapElem) ->
    lists:foldl(fun
        (E, AccSum) when E > 0 -> AccSum + E;
        (E, AccSum) when E < 0 -> AccSum - E
    end, 0, MapElem).
    
count_length_map(Map) ->
    lists:foldl(fun(MapElem, AccSum) ->
        Sum = count_length_map_elem(MapElem),
        AccSum + Sum
    end, 0, Map).

count_length_code(Code) ->
    lists:sum(Code) + erlang:length(Code) - 1.



print_springs([]) -> ok;
print_springs([{S,C}|OtherRows]) ->
    io:fwrite("Springs ~p <- ~p~n", [S, C]),
    print_springs(OtherRows).


trim_ending_newline(Str) ->
    string:sub_string(Str, 1, string:len(Str) - 1).
        
    
numbers_str_to_list(NumbersStr) ->
    NumbersStrList = string:split(NumbersStr, ",", all),
    lists:filtermap(fun
        ("")        -> false;
        (NumberStr) -> {true, erlang:list_to_integer(NumberStr)}
    end, NumbersStrList).


split_by_count(List, Count) ->
    split_by_count(Count, List, []).

split_by_count(0,          Remaining,  First) -> {lists:reverse(First), Remaining};
split_by_count(Count, [E | Remaining], First) -> split_by_count(Count-1, Remaining, [E | First]).


fact(N)      -> fact(N, 1).
fact(0, Acc) -> Acc;
fact(N, Acc) -> fact(N-1, Acc*N).
