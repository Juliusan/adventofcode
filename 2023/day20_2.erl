-module(day20_2).
-export([solve/1]).

% Galvojau bus labai paprasta. Pasirodo - ne. Atsakymas penkiaženklis, o tiek ciklų neprasuksi.
% Šį kartą tiesiog pažiūrėjau į duomenis ir pamąsčiau, kaip galėčiau gauti atsakymą. Tiesa, toks
% atsakymas galėjo gautis tik su keliomis prielaidomis: kad visas tinklas pasidalina į kelis
% nepriklausomus potinklius. Kad potinkliuose iki aktyvuojamas tinkamas nodas nėra ciklų (bet
% šitas gal ir akivaizdu). Jau tik pateikęs atsakymą patvarkiau kodą, kad jis patikrintų mano
% prielaidas ir suskaičiuotų atsakymą pats. UPDATE: pasirodo, (ir vėl) buvau pamiršęs patikrinti,
% ar radus reikiamą ėjimų skaičių pomedis ciklinasi. Pridėjau tą patikrinimą.

solve(FileName) ->
    Modules = get_input(FileName),
    ModulesFilled = fill_conjunctions(Modules),
    ModulesPartitioned = partition_modules(ModulesFilled),
    Count = run_partitions(ModulesPartitioned),
    Count.
    
    
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
    
    
partition_modules(Modules) ->
    % &dr -> rx
    #{bcast := {bcast, undefined, Destinations}} = Modules,
    PartsOuts = lists:map(fun(Destination) ->
        filter_modules(Modules, undefined, Destination, [], [])
    end, Destinations),
    Parts = lists:map(fun({Ds, [_]}) -> Ds end, PartsOuts),
    true = check_intersections(Parts),
    %io:fwrite("XXX ~p~n", [Parts]),
    PartsSources = lists:zip(Parts, Destinations),
    Partitions = lists:map(fun({Part, Source}) ->
        BasicModules = maps:filter(fun(Name, _) -> lists:member(Name, Part) end, Modules),
        BasicModules#{bcast => {bcast, undefined, [Source]}}
    end, PartsSources),
    %io:fwrite("XXX ~p~n", [Partitions]),
    Partitions.
    
    
filter_modules(_Modules, FName, "dr", AccDestinations, Outs) ->
    {AccDestinations, [FName|Outs]};

filter_modules(Modules, _FName, DName, AccDestinations, Outs) ->
    case lists:member(DName, AccDestinations) of
        true  -> {AccDestinations, Outs};
        false ->
            NewAccDestinations = [DName | AccDestinations],
            case maps:find(DName, Modules) of
                error ->
                    {NewAccDestinations, Outs};
                {ok, {_, _, Destinations}} ->
                    lists:foldl(fun(Destination, {AccDs, AccOs}) ->
                        filter_modules(Modules, DName, Destination, AccDs, AccOs)
                    end, {NewAccDestinations, Outs}, Destinations)
            end
    end.
    
    
check_intersections([_])                  -> true;
check_intersections([First,Second|Other]) -> 
    case check_intersection(First, [Second|Other]) of
        true  -> check_intersections([Second|Other]);
        false -> false
    end.
    
check_intersection(_,     [             ]) -> true;
check_intersection(First, [Second|Others]) ->
    %io:fwrite("XXX ~p -> ~p~n", [First, Second]),
    case lists:any(fun(ElemF) -> lists:member(ElemF, Second) end, First) of
        true  -> false;
        false -> check_intersection(First, Others)
    end.
    

run_partitions(Partitions) ->
    run_partitions(Partitions, 1).
    
run_partitions([],                AccResult) -> AccResult;
run_partitions([Part|Partitions], AccResult) ->
    {Result, FinalPart} = run_modules(Part),
    %io:fwrite("XXX ~p~n", [FinalPart]),
    %io:fwrite("XXX ~p~n", [Part]),
    {{ir, [{From, high}], ["dr"]}, FinalPartNoDr} = partition_final(FinalPart),
    {{ir, [{From, low }], ["dr"]}, PartNoDr     } = partition_final(Part),
    PartNoDr = FinalPartNoDr,
    run_partitions(Partitions, Result*AccResult).
    
    
partition_final(Map) ->
    FinalKey = maps:fold(fun
        ( Key, {ir, _, ["dr"]}, undefined) -> Key;
        (_Key, _              , Found    ) -> Found
    end, undefined, Map),
    maps:take(FinalKey, Map).


run_modules(Modules) ->
    run_modules(Modules, 1, []).
    
run_modules(_Modules, Count, _States) when Count > 100000 ->
    false;
    
run_modules(Modules, Count, States) ->
    State = get_state(Modules),
    case lists:member(State, States) of
        true ->
            loop;
        false ->
            case run_module_single(Modules) of
                {true,  NewModules} -> {Count, NewModules};
                {false, NewModules} -> run_modules(NewModules, Count+1, [get_state(Modules)|States])
            end
    end.


get_state(Modules) -> Modules.


run_module_single(Modules) ->
    run_module_single(Modules, [{low, undefined, bcast}], [], false).

run_module_single(Modules, [], [], Found) ->
    {Found, Modules};

run_module_single(Modules, [], NextSignals, Found) ->
    run_module_single(Modules, lists:reverse(NextSignals), [], Found);

run_module_single(Modules, [{high, _SName, "dr"}|Signals], NextSignals, false) ->
    run_module_single(Modules, Signals, NextSignals, true);
    
run_module_single(Modules, [{Type, SName, DName}|Signals], NextSignals, Found) ->
    %io:fwrite("XXX STEP ~p -> (~p) -> ~p~n", [SName, Type, DName]),
    {NM, NNS} = case {maps:find(DName, Modules), Type} of
        {error, _} ->
            {Modules, NextSignals};
        {{ok, {bcast, undefined, Destinations}}, _} ->
            NewNextSignals = send_pulse(Type, DName, Destinations, NextSignals),
            {Modules, NewNextSignals};
        {{ok, {flip, _, _}}, high} ->
            {Modules, NextSignals};
        {{ok, {flip, on, Destinations}}, low} ->
            NewModules = Modules#{DName => {flip, off, Destinations}},
            NewNextSignals = send_pulse(low, DName, Destinations, NextSignals),
            {NewModules, NewNextSignals};
        {{ok, {flip, off, Destinations}}, low} ->
            NewModules = Modules#{DName => {flip, on, Destinations}},
            NewNextSignals = send_pulse(high, DName, Destinations, NextSignals),
            {NewModules, NewNextSignals};
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
            {NewModules, NewNextSignals}
    end,
    run_module_single(NM, Signals, NNS, Found).


send_pulse(_Type, _Source, [],                         NextSignals) -> NextSignals;
send_pulse( Type,  Source, [Destination|Destinations], NextSignals) -> send_pulse(Type, Source, Destinations, [{Type, Source, Destination}|NextSignals]).


trim_ending_newline(Str) ->
    string:sub_string(Str, 1, string:len(Str) - 1).

