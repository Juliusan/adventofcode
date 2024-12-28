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
            {op_and, _,         _        } -> {AccVars#{ResName => utils:bit_and(Op1, Op2)}, AccGates};
            {op_or,  _,         _        } -> {AccVars#{ResName => utils:bit_or( Op1, Op2)}, AccGates};
            {op_xor, _,         _        } -> {AccVars#{ResName => utils:bit_xor(Op1, Op2)}, AccGates}
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
z_names([],     _, Acc) -> lists:reverse(Acc);
z_names([1|Ds], I, Acc) -> z_names(Ds, I+1, [var_name($z, I)|Acc]);
z_names([0|Ds], I, Acc) -> z_names(Ds, I+1, Acc).


var_name(Var, Index) when Index < 10 -> [Var, $0, Index+$0];
var_name(Var, Index)                 -> [Var | erlang:integer_to_list(Index)].


var_info([Var|IndexStr]) -> {Var, erlang:list_to_integer(IndexStr)}.


find_gate_with_input(_In, _Op, []                            ) -> undefined;
find_gate_with_input( In,  Op, [{Op, In, _, _} = Gate|_     ]) -> Gate;
find_gate_with_input( In,  Op, [{Op, _, In, _} = Gate|_     ]) -> Gate;
find_gate_with_input( In,  Op, [_                    | Gates]) -> find_gate_with_input(In, Op, Gates).


find_bad_gates(Gates) ->
    MaxZ = lists:foldl(fun
        ({_, _, _, [$z|_] = GateName}, Acc) ->
            {$z, Int} = var_info(GateName),
            erlang:max(Acc, Int);
        (_, Acc) ->
            Acc
    end, 0, Gates),
    MaxZName = var_name($z, MaxZ),
    %
    % This is true exept if i=00 or i is maximal index of z
    % x_i       XOR y_i         = sum_i
    % x_{i-1}   AND y_{i-1}     = carry1_i
    % sum_{i_1} AND carry_{i-1} = carry2_i
    % carry1_1  OR  carry2_i    = carry_i
    % carry_i   XOR sum_i       = z_i
    %
    {Sums, Carry1s, Carry2s, Carrys, _Zs, Others} = lists:foldl(fun
        ({op_and, Op1, Op2, _} = Gate, {S, C1, C2, C, Z, O}) ->
            case {Op1, Op2} of
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
    BadSums = lists:filter(fun({op_xor, Op1, Op2, Res}) ->
        (
            undefined =:= find_gate_with_input(Res, op_and, Gates) orelse
            undefined =:= find_gate_with_input(Res, op_xor, Gates)
        ) andalso
        Op1 =/= "x00" andalso Op2 =/= "y00" andalso
        Op1 =/= "y00" andalso Op2 =/= "x00"
    end, Sums),
    BadCarry1s = lists:filter(fun({op_and, Op1, Op2, Res}) ->
        undefined =:= find_gate_with_input(Res, op_or, Gates) andalso
        Op1 =/= "x00" andalso Op2 =/= "y00" andalso
        Op1 =/= "y00" andalso Op2 =/= "x00"
    end, Carry1s),
    BadCarry2s = lists:filter(fun({op_and, _, _, Res}) ->
        undefined =:= find_gate_with_input(Res, op_or, Gates)
    end, Carry2s),
    BadCarrys = lists:filter(fun({op_or, _, _, Res}) ->
        undefined =:= find_gate_with_input(Res, op_xor, Gates) andalso
        Res =/= MaxZName
    end, Carrys),
    GetResultFun = fun(List) -> lists:map(fun({_,_,_,Res}) -> Res end, List) end,
    GetResultFun(Others) ++
        GetResultFun(BadSums) ++
        GetResultFun(BadCarry1s) ++
        GetResultFun(BadCarry2s) ++
        GetResultFun(BadCarrys).


find_swaps(Inits, Gates, Opts) ->
    X = utils:bits_to_integer(get_full_var($x, Inits)),
    Y = utils:bits_to_integer(get_full_var($y, Inits)),
    SumBits = utils:integer_to_bits(X+Y),
    GateMap = lists:foldl(fun({_, _, _, ResName} = Gate, Acc) ->
        Acc#{ResName => Gate}
    end, #{}, Gates),
    find_swaps(Inits, GateMap, SumBits, Opts, []).


find_swaps(Inits, GateMap, SumBits, [], Acc) ->
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

find_swaps(Inits, GateMap, SumBits, [Gate1Name|OtherGates], Acc) ->
    lists:foldl(fun
        (Gate2Name, undefined) ->
            {Op1, Op11, Op12, Gate1Name} = maps:get(Gate1Name, GateMap),
            {Op2, Op21, Op22, Gate2Name} = maps:get(Gate2Name, GateMap),
            NewGateMap = GateMap#{
                Gate1Name => {Op2, Op21, Op22, Gate1Name},
                Gate2Name => {Op1, Op11, Op12, Gate2Name}
            },
            find_swaps(Inits, NewGateMap, SumBits, OtherGates -- [Gate2Name], [{Gate1Name, Gate2Name}|Acc]);
        (_Gate2Name, Result) ->
            Result
    end, undefined, OtherGates).


solve_1(FileName) ->
    {Inits, Gates} = read_inputs(FileName),
    Vars = calculate_gates(Inits, Gates),
    utils:bits_to_integer(get_full_var($z, Vars)).


solve_2(FileName) ->
    {Inits, Gates} = read_inputs(FileName),
    BadGates = find_bad_gates(Gates),
    2*4 = erlang:length(BadGates),
    [_,_,_,_] = find_swaps(Inits, Gates, BadGates),   % Just a check
    string:join(lists:usort(BadGates), ",").
