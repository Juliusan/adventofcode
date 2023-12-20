-module(day20_1).
-export([solve/1]).

% Daug programavimo, bet šiaip tai nieko sudėtingo.

solve(FileName) ->
    Modules = get_input(FileName),
    io:fwrite("XXX ~p~n", [Modules]),
    ModulesFilled = fill_conjunctions(Modules),
    io:fwrite("XXX ~p~n", [ModulesFilled]),
    {Lows, Highs} = run_modules(ModulesFilled, 1000),
    Lows*Highs.
    
    
get_input(FileName) ->
    {ok, File} = file:open(FileName, [read]),
    Modules = get_input_modules(File, #{}),
    ok = file:close(File),
    Modules.

    
get_input_modules(File, AccModules) ->
    case file:read_line(File) of
        eof ->
            AccModules;
        {ok, Line} ->
            %io:fwrite("XXX ~p: ", [Line]),
            ModuleLine = trim_ending_newline(Line),
            {Name, Config} = get_input_module(ModuleLine),
            NewAccModules = AccModules#{Name => Config},
            get_input_modules(File, NewAccModules)
    end.
    
    
get_input_module(Line) ->
    [TypeName, DestinationsStr] = string:split(Line, " -> "),
    Destinations = string:split(DestinationsStr, ", ", all),
    case TypeName of
        "broadcaster" -> {bcast, {bcast, undefined, Destinations}};
        [$% | Name]   -> {Name, {flip, off, Destinations}};
        [$& | Name]   -> {Name, {ir, undefined, Destinations}}
    end.
    
    
fill_conjunctions(Modules) ->
    maps:fold(fun
        (IrName, {ir, undefined, IrDestinations}, AccModules) ->
            Sources = maps:fold(fun(Name, {_T, _S, Destinations}, AccSources) ->
                case lists:member(IrName, Destinations) of
                    true  -> [{Name, low}|AccSources];
                    false -> AccSources
                end
            end, [], Modules),
            AccModules#{IrName => {ir, lists:sort(Sources), IrDestinations}};
        (_, {_, _, _}, AccModules) ->
            AccModules
    end, Modules, Modules).
    
    
get_state(Modules) -> Modules.


%is_same_state(State1, State2) -> State1 =:= State2.


run_modules(Modules, Count) ->
    run_modules(Modules, Count, Count, []).
    
run_modules(_Modules, 0, _InitCount, States) ->
    io:fwrite("XXX NO LOOP~n"),
    sum_counts(States);
    
run_modules(Modules, Count, InitCount, States) ->
    State = get_state(Modules),
    case find_state(State, States) of
        false ->
            {NewModules, Counts} = run_module_single(Modules, {1, 0}, [{low, undefined, bcast}], []),
            io:fwrite("XXX COUNT ~p (~p)~n", [Counts, Count]),
            NewStates = [{State, Counts}|States],
            Result = run_modules(NewModules, Count-1, InitCount, NewStates),
            Result;
        {LoopStates, InitStates} ->
            io:fwrite("XXX LOOP~n"),
            LoopCount = InitCount - erlang:length(InitStates),
            Loops = LoopCount div erlang:length(LoopStates),
            LoopEndCount = LoopCount rem erlang:length(LoopStates),
            LoopEndStates = firstn(LoopEndCount, LoopStates),
            io:fwrite("XXX LOOP COUNT ~p ~p ~p~n", [LoopCount, Loops, LoopEndCount]),
            InitSum             = sum_counts(InitStates),
            {LoopLow, LoopHigh} = sum_counts(LoopStates),
            LoopEndSum          = sum_counts(LoopEndStates),
            sum_counts([{undefined, InitSum}, {undefined, {Loops*LoopLow, Loops*LoopHigh}}, {undefined, LoopEndSum}])
    end.


run_module_single(Modules, AccCounts, [], []) ->
    {Modules, AccCounts};

run_module_single(Modules, AccCounts, [], NextSignals) ->
    run_module_single(Modules, AccCounts, lists:reverse(NextSignals), []);
    
run_module_single(Modules, AccCounts, [{Type, SName, DName}|Signals], NextSignals) ->
    %io:fwrite("XXX STEP ~p -> (~p) -> ~p: ~p~n", [SName, Type, DName, AccCounts]),
    {NM, NAC, NNS} = case {maps:find(DName, Modules), Type} of
        {error, _} ->
            {Modules, AccCounts, NextSignals};
        {{ok, {bcast, undefined, Destinations}}, _} ->
            NewNextSignals = send_pulse(Type, DName, Destinations, NextSignals),
            NewAccCounts = add_count(Type, erlang:length(Destinations), AccCounts), 
            {Modules, NewAccCounts, NewNextSignals};
        {{ok, {flip, _, _}}, high} ->
            {Modules, AccCounts, NextSignals};
        {{ok, {flip, on, Destinations}}, low} ->
            NewModules = Modules#{DName => {flip, off, Destinations}},
            NewNextSignals = send_pulse(low, DName, Destinations, NextSignals),
            NewAccCounts = add_count(low, erlang:length(Destinations), AccCounts), 
            {NewModules, NewAccCounts, NewNextSignals};
        {{ok, {flip, off, Destinations}}, low} ->
            NewModules = Modules#{DName => {flip, on, Destinations}},
            NewNextSignals = send_pulse(high, DName, Destinations, NextSignals),
            NewAccCounts = add_count(high, erlang:length(Destinations), AccCounts), 
            {NewModules, NewAccCounts, NewNextSignals};
        {{ok, {ir, Memory, Destinations}}, _} ->
            %io:fwrite("  XXX STEP AND ~p~n", [Memory]),
            {value, {SName, OldNMemory}, MemoryNoN} = lists:keytake(SName, 1, Memory),
            NewMemory = case OldNMemory =:= Type of
                true  -> Memory;
                false -> lists:sort([{SName, Type}|MemoryNoN])
            end,
            NewModules = Modules#{DName => {ir, NewMemory, Destinations}},
            TypeToSend = case lists:all(fun({_, T}) -> T =:= high end, NewMemory) of
                true  -> low;
                false -> high
            end,
            NewNextSignals = send_pulse(TypeToSend, DName, Destinations, NextSignals),
            NewAccCounts = add_count(TypeToSend, erlang:length(Destinations), AccCounts), 
            {NewModules, NewAccCounts, NewNextSignals}
    end,
    run_module_single(NM, NAC, Signals, NNS).
                
    
find_state(State, States) -> find_state(State, States, []).
find_state(_,     [],                     _FirstStates) -> false;
find_state(State, [{State, Count}|States], FirstStates) -> {[{State, Count}|FirstStates], States};
find_state(State, [  StateCount  |States], FirstStates) -> find_state(State, States, [StateCount|FirstStates]).


send_pulse(_Type, _Source, [],                         NextSignals) -> NextSignals;
send_pulse( Type,  Source, [Destination|Destinations], NextSignals) -> send_pulse(Type, Source, Destinations, [{Type, Source, Destination}|NextSignals]).


add_count(low,  Count, {Low, High}) -> {Low+Count, High};
add_count(high, Count, {Low, High}) -> {Low, High+Count}.


sum_counts(States) -> sum_counts(States, {0, 0}).
sum_counts([],                             AccSums          ) -> AccSums;
sum_counts([{_State, {Low, High}}|States], {AccLow, AccHigh}) -> sum_counts(States, {AccLow+Low, AccHigh+High}).



trim_ending_newline(Str) ->
    string:sub_string(Str, 1, string:len(Str) - 1).
    
    
firstn(Count, List) ->
    {First, _} = split_by_count(Count, List, []),
    First.

split_by_count(0,          Remaining,  First) -> {lists:reverse(First), Remaining};
split_by_count(Count, [E | Remaining], First) -> split_by_count(Count-1, Remaining, [E | First]).

