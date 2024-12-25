-module(day24).
-export([solve_1/1, solve_2/1]).

% Pirma dalis iš tiesų buvo nesudėtinga. Padariau ją be didesnių problemų. O su
% antra dalimi strigau. Iš pradžių bandžiau daryti porų perrinkimą. Supratau,
% kad pilnas perrinkimas nepaeis, tai susiskaičiavau blogus bitus, pažiūrėjau
% nuo kurių kintamųjų jie priklauso ir poras rinkau tik iš jų. Tiesa, jų vis
% tiek buvo, berods, 170. Kad optimizuočiau, padariau prielaidą, kad kiekvienas
% pakeitimas turi ištaisyti bent vieną klaidą ir nepadaryti naujų. Deja, tokiu
% atveju atsakymo rasti nepavyko. Tada, žinoma, išbandžiau ir pilną perrinkimą,
% kuris, kaip ir turėjo būti, sukosi labai ilgai. Po kurio laiko kilo mintis
% pasianalizuoti ne bendrai nuo ko priklauso visi blogi bitai, kiekvienas
% atskirai. Pastebėjau, kad kuo reikšmingesnis bitas, tuo didesnis kintamųjų
% sąrašas. Ir dar daugiau - žemesnio bito sąrašas yra aukštesnio bito sąrašo
% poaibis. Tada nusprendžiau kiek pasianalizuoti užduotį ir gana greitai
% supratau, kad iš tiesų tai yra tiesiog paprasta sudėtis užrašyta bitų
% operacijomis. Pasidarė aišku, kad taisyti reikia nuo žemiausių bitų. Taigi
% susižiūrėjau, kokios operacijos atliekamos einant nuo žemesnio bitų prie
% reikšmingesnio, išsitraukiau visas žemiausio blogo bito operacijas, bet taip
% ir nesugalvojau, kaip rasti, į ką jos turi būti keičiamos. Grižau prie porų
% paieškos tik pradedant nuo žemiausio blogo bito ir eliminuojant žemesnių bitų
% kintamuosius. Bet tų pasirinkimų vis tiek pasirodė per daug. Na ir tada atėjo
% išganinga mintis - susigrupuoti visas operacijas (vartus) pagal tipą ir
% pažiūrėti, kurios paprasčiausiai neatitinka šablono. Tokių operacijų gavau
% 11. Buvo aišku, kad tos, kurių kintamieji yra x00 ir y00 yra geros, tiesiog
% sudėties pradžioje šablonai yra kitokie. Vis tiek liko 9. Tai tiesiog
% perrinkau kombinacijas ir taip vieną iš jų atmečiau. Nors tiksliau, padariau
% progamoje klaidą, kuri leido teisingai atmesti vieną kintąmąjį, tai tiesiog
% suradau, kuri 8 registrų apkeitimų kombinacija duoda teisingą rezultatą.
% Pasirodo, sudėties pabaigoje šablonai taip pat kitokie.

% (aoc_2024@JuliusErisataT14.erisata.lt)1> day24:solve_1("priv/day24-PVZ1.txt").
% 4
% (aoc_2024@JuliusErisataT14.erisata.lt)2> day24:solve_1("priv/day24-PVZ2.txt").
% 2024
% (aoc_2024@JuliusErisataT14.erisata.lt)3> day24:solve_1("priv/day24.txt").
% 49574189473968
% (aoc_2024@JuliusErisataT14.erisata.lt)4> day24:solve_2("priv/day24.txt").
% "ckb,kbs,ksv,nbd,tqq,z06,z20,z39"
% (aoc_2024@JuliusErisataT14.erisata.lt)5> timer:tc(fun() -> day24:solve_1("priv/day24.txt") end).
% {2341,49574189473968}
% (aoc_2024@JuliusErisataT14.erisata.lt)6> timer:tc(fun() -> day24:solve_2("priv/day24.txt") end).
% {95006,"ckb,kbs,ksv,nbd,tqq,z06,z20,z39"}


read_inputs(FileName) ->
    [Inits,Gates] = utils:read_lines_no_new_line_to_elems(FileName, [
        fun(Line) ->
            [Name, Bit] = string:split(Line, ": ", all),
            BitInt = case Bit of
                "0" -> 0;
                "1" -> 1
            end,
            {Name, BitInt}
        end,
        fun(Line) ->
            [Op1, OpStr, Op2, "->", Res] = string:split(Line, " ", all),
            Op = case OpStr of
                "AND" -> op_and;
                "OR"  -> op_or;
                "XOR" -> op_xor
            end,
            {Op, Op1, Op2, Res}
    end], "\n"),
    InitsMap = maps:from_list(Inits),
    {InitsMap, Gates}.


calculate_gates(Vars, []) ->
    Vars;

calculate_gates(Vars, Gates) ->
    {NewVars, NewGates} = lists:foldl(fun({Op, Op1Name, Op2Name, ResName} = Gate, {AccVars, AccGates}) ->
        Op1 = maps:get(Op1Name, AccVars, undefined),
        Op2 = maps:get(Op2Name, AccVars, undefined),
        case {Op, Op1, Op2} of
            {_,      undefined, _        } -> {AccVars,                           [Gate | AccGates]};
            {_,      _,         undefined} -> {AccVars,                           [Gate | AccGates]};
            {op_and, _,         _        } -> {AccVars#{ResName => Op1 band Op2}, AccGates};
            {op_or,  _,         _        } -> {AccVars#{ResName => Op1 bor Op2},  AccGates};
            {op_xor, _,         _        } -> {AccVars#{ResName => Op1 bxor Op2}, AccGates}
        end
    end, {Vars, []}, Gates),
    case NewVars =:= Vars of
        true  -> loop;
        false -> calculate_gates(NewVars, NewGates)
    end.


get_full_var(Name, Vars) ->
    ZMap = maps:filtermap(fun
        ([N|_], Value) when N =:= Name -> {true, Value};
        (_,     _    )                 -> false
    end, Vars),
    ZNameDigits = lists:sort(maps:to_list(ZMap)),
    [ Digit || {_, Digit} <- ZNameDigits ].


z_names(Bits) -> z_names(Bits, 0, []).
z_names([],     _, Acc)             -> lists:reverse(Acc);
z_names([1|Ds], I, Acc) when I < 10 -> z_names(Ds, I+1, [[$z, $0, I+$0]|Acc]);
z_names([1|Ds], I, Acc)             -> z_names(Ds, I+1, [[$z|erlang:integer_to_list(I)]|Acc]);
z_names([0|Ds], I, Acc)             -> z_names(Ds, I+1, Acc).


var_name(Var, Index) when Index < 10 -> [Var, $0, Index+$0];
var_name(Var, Index)                 -> [Var | erlang:integer_to_list(Index)].


var_info([Var|IndexStr]) -> {Var, erlang:list_to_integer(IndexStr)}.


x_or_y([$x|_]) -> true;
x_or_y([$y|_]) -> true;
x_or_y(_     ) -> false.


get_all_opts(RNames, GateMap) ->
    get_all_opts(RNames, RNames, GateMap).

get_all_opts([], AccFinal, _GateMap) ->
    lists:usort(AccFinal);

get_all_opts(RNames, AccFinal, GateMap) ->
    %utils:print("OPS: ~p", [RNames]),
    NewRNames = lists:foldl(fun(RName, Acc) ->
        {_, Op1, Op2, _} = maps:get(RName, GateMap),
        TmpAcc = case {lists:member(Op1, AccFinal), x_or_y(Op1)} of
            {true,  _    } -> Acc;
            {false, true } -> Acc;
            {false, false} -> [Op1|Acc]
        end,
        R = case {lists:member(Op2, AccFinal), x_or_y(Op2)} of
            {true,  _    } -> TmpAcc;
            {false, true } -> TmpAcc;
            {false, false} -> [Op2|TmpAcc]
        end,
        %utils:print("OPS: ~p ? ~p = ~p", [Op1, Op2, RName]),
        R
    end, [], RNames),
    get_all_opts(NewRNames, AccFinal ++ NewRNames, GateMap).


bit_count(Bits) ->
    utils:list_filter_count(fun(1) -> true; (0) -> false end, Bits).


find_op(Op, Op1, GateMap) ->
    Keys = maps:keys(GateMap),
    [OpKey] = lists:filter(fun
        ({O, O1, _}) when O =:= Op, O1 =:= Op1 -> true;
        %({O, O2, _}) when O =:= Op, O2 =:= Op1Or2 -> true;
        ({_, _ , _})                              -> false
    end, Keys),
    Res = maps:get(OpKey, GateMap),
    {Op, Op1, Op2} = OpKey,
    {Op2, Res}.


find_gate_with_input(_In, _Op, []                )        -> undefined;
find_gate_with_input( In,  Op, [{Op, In, _, _} = Gate|_]) -> Gate;
find_gate_with_input( In,  Op, [{Op, _, In, _} = Gate|_]) -> Gate;
find_gate_with_input( In,  Op, [_        | Gates]) -> find_gate_with_input(In, Op, Gates).


find_swaps(Inits, Gates, MaxLength) ->
    {Sums, Carry1s, Carry2s, Carrys, Zs, Others} = lists:foldl(fun
        ({op_and, Op1, Op2, _} = Gate, {S, C1, C2, C, Z, O}) ->
            case {Op1, Op2} of
                % {[$x|_], [$y|_]} -> {S, [{Op1, Op2, Gate}|C1], C2, C, Z, O};
                % {[$y|_], [$x|_]} -> {S, [{Op2, Op1, Gate}|C1], C2, C, Z, O};
                {[$x|_], [$y|_]} -> {S, [Gate|C1], C2, C, Z, O};
                {[$y|_], [$x|_]} -> {S, [Gate|C1], C2, C, Z, O};
                {[$x|_], _     } -> fail;
                {[$y|_], _     } -> fail;
                {_,      [$x|_]} -> fail;
                {_,      [$y|_]} -> fail;
                {_     , _     } -> {S, C1, [Gate|C2], C, Z, O}
            end;
        ({op_or, _, _, _} = Gate, {S, C1, C2, C, Z, O}) ->
            {S, C1, C2, [Gate|C], Z, O};
        ({op_xor, Op1, Op2, Res} = Gate, {S, C1, C2, C, Z, O}) ->
            case {Op1, Op2, Res} of
                % {[$x|_], [$y|_], _     } -> {[{Op1, Op2, Gate}|S], C1, C2, C, Z, O};
                % {[$y|_], [$x|_], _     } -> {[{Op2, Op1, Gate}|S], C1, C2, C, Z, O};
                {[$x|_], [$y|_], _     } -> {[Gate|S], C1, C2, C, Z, O};
                {[$y|_], [$x|_], _     } -> {[Gate|S], C1, C2, C, Z, O};
                {[$x|_], _     , _     } -> fail;
                {[$y|_], _     , _     } -> fail;
                {_,      [$x|_], _     } -> fail;
                {_,      [$y|_], _     } -> fail;
                {_ ,      _    , [$z|_]} -> {S, C1, C2, C, [Gate|Z], O};
                {_ ,      _    , _     } -> {S, C1, C2, C, Z, [Gate|O]}
            end
    end, {[], [], [], [], [], []}, Gates),
    %utils:print("BadRules ~p", [Others]),
    BadSums = lists:filter(fun({op_xor, Op1, Op2, Res}) ->
        (
            undefined =:= find_gate_with_input(Res, op_and, Gates) orelse
            undefined =:= find_gate_with_input(Res, op_xor, Gates)
        ) andalso
        Op1 =/= "x00" andalso Op2 =/= "y00" andalso
        Op1 =/= "y00" andalso Op2 =/= "x00"
    end, Sums),
    %utils:print("BadSums ~p", [BadSums]),
    BadCarry1s = lists:filter(fun({op_and, Op1, Op2, Res}) ->
        undefined =:= find_gate_with_input(Res, op_or, Gates) andalso
        Op1 =/= "x00" andalso Op2 =/= "y00" andalso
        Op1 =/= "y00" andalso Op2 =/= "x00"
    end, Carry1s),
    %utils:print("BadCarry1s ~p", [BadCarry1s]),
    BadCarry2s = lists:filter(fun({op_and, _, _, Res}) ->
        undefined =:= find_gate_with_input(Res, op_or, Gates)
    end, Carry2s),
    %utils:print("BadCarry2s ~p", [BadCarry2s]),
    BadCarrys = lists:filter(fun({op_or, _, _, Res}) ->
        undefined =:= find_gate_with_input(Res, op_xor, Gates)
    end, Carrys),
    %utils:print("BadCarrys ~p", [BadCarrys]),
    GetOptsFun = fun(List) -> lists:map(fun({_,_,_,Res}) -> Res end, List) end,
    Opts =
        GetOptsFun(Others) ++
        GetOptsFun(BadSums) ++
        GetOptsFun(BadCarry1s) ++
        GetOptsFun(BadCarry2s) ++
        GetOptsFun(BadCarrys),
    %utils:print("Opts: ~p", [Opts]),
    find_swaps(Inits, Gates, Opts, MaxLength).


find_swaps(Inits, Gates, Opts, MaxLength) ->
    X = utils:bits_to_integer(get_full_var($x, Inits)),
    Y = utils:bits_to_integer(get_full_var($y, Inits)),
    SumBits = utils:integer_to_bits(X+Y),
    GateMap = lists:foldl(fun({_, _, _, ResName} = Gate, Acc) ->
        Acc#{ResName => Gate}
    end, #{}, Gates),
    find_swaps(Inits, GateMap, SumBits, Opts, MaxLength, []).

find_swaps(Inits, GateMap, SumBits, Opts, MaxLength, Acc) ->
    %utils:print("STEP ~p", [Acc]),
    case erlang:length(Acc) of
        L when L < 2*MaxLength ->
            [Gate1Name|OtherGates] = Opts,
            lists:foldl(fun
                (Gate2Name, undefined) ->
                    {Op1, Op11, Op12, Gate1Name} = maps:get(Gate1Name, GateMap),
                    {Op2, Op21, Op22, Gate2Name} = maps:get(Gate2Name, GateMap),
                    NewGateMap = GateMap#{
                        Gate1Name => {Op2, Op21, Op22, Gate1Name},
                        Gate2Name => {Op1, Op11, Op12, Gate2Name}
                    },
                    find_swaps(Inits, NewGateMap, SumBits, Opts -- [Gate1Name, Gate2Name], MaxLength, [Gate1Name, Gate2Name|Acc]);
                (_Gate2Name, Result) ->
                    Result
            end, undefined, OtherGates);
        L when L =:= 2*MaxLength ->
            case calculate_gates(Inits, maps:values(GateMap)) of
                loop ->
                    undefined;
                Vars ->
                    ZBits = get_full_var($z, Vars),
                    Diff = utils:bits_xor(ZBits, SumBits),
                    ZNames = z_names(Diff),
                    case ZNames of
                        []    -> Acc;
                        [_|_] -> undefined
                    end
            end;
        _ ->
            undefined
    end.


% find_swaps(Inits, Gates, MaxLength) ->
%     X = utils:bits_to_integer(get_full_var($x, Inits)),
%     Y = utils:bits_to_integer(get_full_var($y, Inits)),
%     SumBits = utils:integer_to_bits(X+Y),
%     GateMap = lists:foldl(fun({_, _, _, ResName} = Gate, Acc) ->
%         Acc#{ResName => Gate}
%     end, #{}, Gates),
%     find_swaps(Inits, GateMap, SumBits, MaxLength, []).

% find_swaps(Inits, GateMap, SumBits, MaxLength, Acc) ->
%     utils:print("STEP ~p", [Acc]),
%     case calculate_gates(Inits, maps:values(GateMap)) of
%         loop ->
%             undefined;
%         Vars ->
%             ZBits = get_full_var($z, Vars),
%             Diff = utils:bits_xor(ZBits, SumBits),
%             ZNames = z_names(Diff),
%             case ZNames of
%                 [] ->
%                     case erlang:length(Acc) of
%                         L when L =:= 2*MaxLength -> Acc;
%                         _                        -> undefined
%                     end;
%                 [ZName|_] ->
%                     case erlang:length(Acc) of
%                         L when L < 2*MaxLength ->
%                             {$z, Index} = var_info(ZName),
%                             ZNamePrev = var_name($z, Index-1),
%                             ZOptsPrev = get_all_opts([ZNamePrev], GateMap),
%                             ZOpts = (get_all_opts([ZName], GateMap) -- ZOptsPrev) -- Acc,
%                             AllOpts = ((get_all_opts(ZNames, GateMap) -- ZOptsPrev) -- Acc) -- [ZName],
%                             lists:foldl(fun
%                                 (Gate1Name, undefined) ->
%                                     lists:foldl(fun
%                                         (Gate2Name, undefined) ->
%                                             {Op1, Op11, Op12, Gate1Name} = maps:get(Gate1Name, GateMap),
%                                             {Op2, Op21, Op22, Gate2Name} = maps:get(Gate2Name, GateMap),
%                                             NewGateMap = GateMap#{
%                                                 Gate1Name => {Op2, Op21, Op22, Gate1Name},
%                                                 Gate2Name => {Op1, Op11, Op12, Gate2Name}
%                                             },
%                                             case calculate_gates(Inits, maps:values(NewGateMap)) of
%                                                 loop -> undefined;
%                                                 NewVars ->
%                                                     NewZBits = get_full_var($z, NewVars),
%                                                     NewDiff = utils:bits_xor(NewZBits, SumBits),
%                                                     NewZNames = z_names(NewDiff),
%                                                     case lists:member(ZName, NewZNames) of
%                                                         true  -> undefined;
%                                                         false -> find_swaps(Inits, NewGateMap, SumBits, MaxLength, [Gate1Name, Gate2Name | Acc])
%                                                     end
%                                             end;
%                                         (_GateName2, Result) ->
%                                             Result
%                                     end, undefined, AllOpts);
%                                 (_GateName1, Result) ->
%                                     Result
%                             end, undefined, ZOpts);
%                         _ ->
%                             undefined
%                     end
%             end
%     end.


% find_swaps(Inits, Gates, MaxLength) ->
%     X = utils:bits_to_integer(get_full_var($x, Inits)),
%     Y = utils:bits_to_integer(get_full_var($y, Inits)),
%     SumBits = utils:integer_to_bits(X+Y),
%     GateMap = lists:foldl(fun({_, _, _, ResName} = Gate, Acc) ->
%         Acc#{ResName => Gate}
%     end, #{}, Gates),
%     Vars = calculate_gates(Inits, Gates),
%     ZBits = get_full_var($z, Vars),
%     Diff = utils:bits_xor(ZBits, SumBits),
%     ZNames = z_names(Diff),
%     lists:foldl(fun(ZName, {AccSwaps, AccGateMap}) ->
%         {$z, Index} = var_info(ZName),
%         Sum                 = maps:get({op_xor, var_name($x, Index  ), var_name($y, Index  )}, AccGateMap),
%         Carry1              = maps:get({op_and, var_name($x, Index-1), var_name($y, Index-1)}, AccGateMap),
%         SumPrev             = maps:get({op_xor, var_name($x, Index-1), var_name($y, Index-1)}, AccGateMap),
%         {CarryPrev, Carry2} = find_op(  op_and, SumPrev,                                       AccGateMap),
%         Carry               = maps:get({op_or,  Carry1,                Carry2},                AccGateMap),
%         %utils:print("~p ~p ~p ~p ~p ~p ", [SumPrev, CarrySum, Carry1, Carry2]),
%         2 = 3
%     end, {[], GateMap}, ZNames).


% find_swaps(Inits, Gates, MaxLength) ->
%     X = utils:bits_to_integer(get_full_var($x, Inits)),
%     Y = utils:bits_to_integer(get_full_var($y, Inits)),
%     SumBits = utils:integer_to_bits(X+Y),
%     %utils:print("X ~p Y ~p", [X, Y]),
%     GateMap = lists:foldl(fun({_, _, _, ResName} = Gate, Acc) ->
%         Acc#{ResName => Gate}
%     end, #{}, Gates),
%     Vars = calculate_gates(Inits, Gates),
%     ZBits = get_full_var($z, Vars),
%     Diff = utils:bits_xor(ZBits, SumBits),
%     %utils:print("XOR ~p ~p ~p", [ZBits, SumBits, Diff]),
%     DiffBitCount = bit_count(Diff),
%     {Result, _} = find_swaps(Diff, DiffBitCount, X+Y, SumBits, Inits, GateMap, MaxLength, [], #{}),
%     Result.


% find_swaps(Diff, DiffBitCount, Sum, SumBits, Inits, GateMap, MaxLength, AccResult, AccChecked) ->
%     %utils:print("ITER ~p ~p", [DiffBitCount, Acc]),
%     case erlang:length(AccResult) of
%         L when L >= 2*MaxLength ->
%             {undefined, AccChecked};
%         _ ->
%             ZNames = z_names(Diff),
%             %utils:print("ZNAMES ~p ~p", [Diff, ZNames]),
%             Opts = get_all_opts(ZNames, GateMap),
%             OptsToSearch = Opts -- AccResult,
%             %utils:print("Opts ~p", [OptsToSearch]),
%             utils:foldl_pairs(fun
%                 (Gate1Name, Gate2Name, {undefined, Acc}) ->
%                     %utils:print("ITER SWAP ~p ~p ~p", [DiffBitCount, Gate1Name, Gate2Name]),
%                     case maps:get({Gate1Name, Gate2Name}, Acc, undefined) of
%                         true ->
%                             {undefined, Acc};
%                         undefined ->
%                             {Op1, Op11, Op12, Gate1Name} = maps:get(Gate1Name, GateMap),
%                             {Op2, Op21, Op22, Gate2Name} = maps:get(Gate2Name, GateMap),
%                             NewGateMap = GateMap#{
%                                 Gate1Name => {Op2, Op21, Op22, Gate1Name},
%                                 Gate2Name => {Op1, Op11, Op12, Gate2Name}
%                             },
%                             NewAcc = Acc#{
%                                 {Gate1Name, Gate2Name} => true,
%                                 {Gate2Name, Gate1Name} => true
%                             },
%                             case calculate_gates(Inits, maps:values(NewGateMap)) of
%                                 loop ->
%                                     {undefined, NewAcc};
%                                 NewVars ->
%                                     %utils:print("ITER SWAP ~p ~p ~p NEW VARS", [DiffBitCount, Gate1Name, Gate2Name]),
%                                     NewZBits = get_full_var($z, NewVars),
%                                     case Sum =:= utils:bits_to_integer(NewZBits) of
%                                         true  -> ok;
%                                         false ->
%                                             NewDiff = utils:bits_xor(NewZBits, SumBits),
%                                             NewDiffBitCount = bit_count(NewDiff),
%                                             %case NewDiffBitCount < DiffBitCount of
%                                             %    true  ->
%                                             find_swaps(NewDiff, NewDiffBitCount, Sum, SumBits, Inits, NewGateMap, MaxLength, [Gate1Name, Gate2Name | AccResult], NewAcc)%;
%                                             %    false -> {undefined, NewAcc}
%                                             %end
%                                     end
%                             end
%                     end;
%                 (_Gate1Name, _Gate2Name, {Result, Acc}) ->
%                     {Result, Acc}
%             end, {undefined, AccChecked}, OptsToSearch)
%     end.


solve_1(FileName) ->
    {Inits, Gates} = read_inputs(FileName),
    Vars = calculate_gates(Inits, Gates),
    utils:bits_to_integer(get_full_var($z, Vars)).


solve_2(FileName) ->
    {Inits, Gates} = read_inputs(FileName),
    Swap = find_swaps(Inits, Gates, 4),
    string:join(lists:usort(Swap), ",").
