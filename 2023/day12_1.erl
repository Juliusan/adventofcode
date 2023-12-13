-module(day12_1).
-export([solve/1]).
-export([get_splits/2]).

% Reikėjo pagalvoti ir truputį pataisyti kodą.

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
    Springs = string:split(SpringsAll, ".", all),
    SpringsNoEmpty = lists:filter(fun("") -> false; (_) -> true end, Springs),
    Code = numbers_str_to_list(CodeStr),
    {SpringsNoEmpty, Code}.
    
    
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
        io:fwrite("XXX ~p <- ~p:", [Map, Code]),
        Split = lists:duplicate(erlang:length(Map), 0),
        Splits = get_splits(Split, erlang:length(Code)),
        %io:fwrite("~p~n", [Splits]),
        NewSum = lists:foldl(fun(S, AS) ->
            %io:fwrite("  XXX ~p ~p~n", [S, AS]),
            {[], CodeSplit} = lists:foldl(fun(C, {R, ARes}) ->
                %io:fwrite("      XXX ~p ~p ~p~n", [C, R, ARes]),
                {NewRes, NewR} = split_by_count(R, C),
                %io:fwrite("      XXX ~p ~p -> ~p ~p~n", [R, C, NewRes, NewR]),
                {NewR, [NewRes | ARes]}
            end, {Code, []}, S),
            MapCodes = lists:zip(Map, lists:reverse(CodeSplit)),
            NewS = lists:foldl(fun({M, C}, AccOptions) ->
                %io:fwrite("  XXX ~p ~p~n", [M, C]),
                Options = count_options(M, C),
                AccOptions * Options
            end, 1, MapCodes),
            %io:fwrite("  XXX ~p ~p -> ~p~n", [S, AS, NewS]),
            NewS + AS
        end, 0, Splits),
        io:fwrite(" ~p~n", [NewSum]),
        NewSum + AccSum
    end, 0, Springs).
    
    
%count_options(Map, Code) -> 1.
count_options([], []) ->
%    io:fwrite("    XXX1 ~p ~p -> 1~n", [[], []]),
    1;

count_options([FMap], []) when FMap < 0 ->
%    io:fwrite("    XXX2 ~p ~p -> 1~n", [[FMap], []]),
    1;

count_options(_Map, []) ->
%    io:fwrite("    XXX8 ~p ~p -> 0~n", [[_Map], []]),
    0;

count_options(Map, Code) ->
    MapSum = lists:foldl(fun
        (E, AccSum) when E > 0 -> AccSum + E;
        (E, AccSum) when E < 0 -> AccSum - E
    end, 0, Map),
    CodeSum = lists:sum(Code),
    case MapSum >= CodeSum of
        true  ->
            count_options2(Map, Code);
        false ->
%            io:fwrite("    XXX3 ~p ~p -> 0~n", [[], _Code]),
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
    
count_options2([FMap | OMap], [FCode | OCode]) when FMap < 0 ->
%    io:fwrite("    XXX7 ~p ~p...~n", [[FMap | OMap], [FCode | OCode]]),
    MapMinus1 = case FMap of
        -1 -> OMap;
        _  -> [FMap+1 | OMap]
    end,
    OptionsDot = count_options(MapMinus1, [FCode | OCode]),
    OptionsSpring = case count_options_consecutive(MapMinus1, FCode-1) of
            false   -> 0;
            NewOMap -> count_options(NewOMap, OCode)

    end,
    Res = OptionsDot + OptionsSpring,
%    io:fwrite("    XXX7 ~p ~p -> ~p~n", [[FMap | OMap], [FCode | OCode], Res]),
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

 
get_splits(_,  N) when N < 0 -> [];
get_splits([], 0) -> [[]];
get_splits([], _) -> [];
get_splits([First|Others], N) ->
    lists:append(lists:map(fun(AddFirst) ->
        NewFirst = First + AddFirst,
        NewOthers = get_splits(Others, N-AddFirst),
        lists:map(fun(S) -> [NewFirst | S] end, NewOthers)
    end, lists:seq(0, N))).


%print_springs([]) -> ok;
%print_springs([{S,C}|OtherRows]) ->
%    io:fwrite("Springs ~p <- ~p~n", [S, C]),
%    print_springs(OtherRows).


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

