-module(day17).
-export([solve_1/1, solve_2/1]).

% Pirma dalis buvo paprasta, tik rašymo daug. Antra dalis... Iš pradžių net
% nežinojau nuo ko pradėti. Bandžiau sukti programą ratu su outputo A iš naujo,
% bet, žinoma, tai rezultato nedavė. Po to bandžiau spėlioti: gal A-1 bus gerai
% ar A+2, ar 2*a... Niekas negelbėjo. Galiausiai nusprendžiau pačio pavyzdžio
% kodą pasianalizuoti. Radau, kad tiek pavyzdy, tiek mano užduoty vienintelis
% jumpas yra gale ir šoka į pradžią. Tada pradėjau analizuoti pačias operacijas.
% Ir tada paaiškėjo, kad jos vykdomos po tris bitus ir pavyzdyje galima gana
% nesunkiai suskaičiuoti, koks turi būti A registras reikalingam outputui. Na
% ir tada pasirašiau uždavinio sprendimo algoritmą konkrečiai savo užduočiai.
% Ir gavau atsakymą. Bet kitai programai jis netiktų, tai jaučiuosi kaip
% mažumėlę sukčiaujantis.

% (aoc_2024@JuliusErisataT14.erisata.lt)1> day17:solve_1("priv/day17-PVZ1.txt").
% FINAL: A=0, B=1, C=9, Address=2
% []
% (aoc_2024@JuliusErisataT14.erisata.lt)2> day17:solve_1("priv/day17-PVZ2.txt").
% FINAL: A=10, B=0, C=0, Address=6
% "0,1,2"
% (aoc_2024@JuliusErisataT14.erisata.lt)3> day17:solve_1("priv/day17-PVZ3.txt").
% FINAL: A=0, B=0, C=0, Address=6
% "4,2,5,6,7,7,7,7,3,1,0"
% (aoc_2024@JuliusErisataT14.erisata.lt)4> day17:solve_1("priv/day17-PVZ4.txt").
% FINAL: A=0, B=26, C=0, Address=2
% []
% (aoc_2024@JuliusErisataT14.erisata.lt)5> day17:solve_1("priv/day17-PVZ5.txt").
% FINAL: A=0, B=44354, C=43690, Address=2
% []
% (aoc_2024@JuliusErisataT14.erisata.lt)6> day17:solve_1("priv/day17-PVZ6.txt").
% FINAL: A=0, B=0, C=0, Address=6
% "4,6,3,5,6,3,5,2,1,0"
% (aoc_2024@JuliusErisataT14.erisata.lt)7> day17:solve_1("priv/day17.txt").
% FINAL: A=0, B=7, C=1, Address=16
% "1,6,3,6,5,6,5,1,7"
% (aoc_2024@JuliusErisataT14.erisata.lt)8> day17:solve_2("priv/day17.txt").
% FINAL: A=0, B=0, C=0, Address=16
% 247839653009594
% (aoc_2024@JuliusErisataT14.erisata.lt)9> timer:tc(fun() -> day17:solve_1("priv/day17.txt") end).
% FINAL: A=0, B=7, C=1, Address=16
% {447,"1,6,3,6,5,6,5,1,7"}
% (aoc_2024@JuliusErisataT14.erisata.lt)10> timer:tc(fun() -> day17:solve_2("priv/day17.txt") end).
% FINAL: A=0, B=0, C=0, Address=16
% {711,247839653009594}


read_inputs(FileName) ->
    [Line5, "", Line3, Line2, Line1] = utils:read_lines_no_new_line(FileName),
    A = read_register(Line1, $A),
    B = read_register(Line2, $B),
    C = read_register(Line3, $C),
    "Program: " ++ ProgramStr = Line5,
    Program = utils:get_integer_list(ProgramStr, ","),
    {A, B, C, Program}.


read_register(Line, Name) ->
    "Register " ++ [Name|Line2] = Line,
    ": " ++ ValueStr = Line2,
    Value = erlang:list_to_integer(ValueStr),
    utils:integer_to_bits(Value).


program_to_map([], _, Acc) -> Acc;
program_to_map([OpCode,Operand|Program], Address, Acc) ->
    Op = case OpCode of
        0 -> adv;
        1 -> bxl;
        2 -> bst;
        3 -> jnz;
        4 -> bxc;
        5 -> out;
        6 -> bdv;
        7 -> cdv
    end,
    NewAcc = Acc#{Address => {Op, utils:integer_to_bits(Operand)}},
    program_to_map(Program, Address+2, NewAcc).


combo_operand([],      _A, _B, _C) -> [];
combo_operand([1],     _A, _B, _C) -> [1];
combo_operand([0, 1],  _A, _B, _C) -> [0, 1];
combo_operand([1, 1],  _A, _B, _C) -> [1, 1];
combo_operand([0,0,1],  A, _B, _C) -> A;
combo_operand([1,0,1], _A,  B, _C) -> B;
combo_operand([0,1,1], _A, _B,  C) -> C.


run(A, B, C, Address, Program, Output) ->
    %utils:print("A=~p, B=~p, C=~p, Address=~p, Output=~p", [A, B, C, Address, Output]),
    DvFun = fun(OperandCombo) ->
        Operand = combo_operand(OperandCombo, A, B, C),
        OperandInt = utils:bits_to_integer(Operand),
        case OperandInt > erlang:length(A) of
            true  -> [];
            false -> lists:nthtail(OperandInt, A)
        end
    end,
    case maps:get(Address, Program, undefined) of
        undefined ->
            utils:print("FINAL: A=~p, B=~p, C=~p, Address=~p", [utils:bits_to_integer(A), utils:bits_to_integer(B), utils:bits_to_integer(C), Address]),
            {A, Output};
        {adv, OperandCombo} ->
            Result = DvFun(OperandCombo),
            run(Result, B, C, Address+2, Program, Output);
        {bxl, Operand} ->
            Result = utils:bits_xor(B, Operand),
            run(A, Result, C, Address+2, Program, Output);
        {bst, OperandCombo} ->
            Operand = combo_operand(OperandCombo, A, B, C),
            Result = lists:sublist(Operand, 3),
            run(A, Result, C, Address+2, Program, Output);
        {jnz, Operand} ->
            AInt = utils:bits_to_integer(A),
            OperandInt = utils:bits_to_integer(Operand),
            case AInt of
                0 -> run(A, B, C, Address+2,  Program, Output);
                _ -> run(A, B, C, OperandInt, Program, Output)
            end;
        {bxc, _Operand} ->
            Result = utils:bits_xor(B, C),
            run(A, Result, C, Address+2, Program, Output);
        {out, OperandCombo} ->
            Operand = combo_operand(OperandCombo, A, B, C),
            Result = lists:sublist(Operand, 3),
            run(A, B, C, Address+2, Program, [utils:bits_to_integer(Result)|Output]);
        {bdv, OperandCombo} ->
            Result = DvFun(OperandCombo),
            run(A, Result, C, Address+2, Program, Output);
        {cdv, OperandCombo} ->
            Result = DvFun(OperandCombo),
            run(A, B, Result, Address+2, Program, Output)
    end.


trace_back_zero_or_one(Outs, TIndex, DIndex, Acc) ->
    case trace_back(Outs, TIndex, Acc#{DIndex => 0}) of
        undefined -> trace_back(Outs, TIndex, Acc#{DIndex => 1});
        Result    -> Result
    end.


trace_back([], _Index, Acc) ->
    First1 = maps:fold(fun
        (I, 1, A) when I > A -> I;
        (_, _, A)            -> A
    end, -1, Acc),
    case First1 =< 3*16 of
        true ->
            Acc;
        false -> undefined
    end;

trace_back([Out|OtherOuts]=Outs, Index, Acc) ->
    %utils:print("~p ~p ~p", [Out, Index, Acc]),
    Digit1 = maps:get(Index,   Acc, undefined),
    Digit2 = maps:get(Index+1, Acc, undefined),
    Digit3 = maps:get(Index+2, Acc, undefined),
    case {Digit1, Digit2, Digit3} of
        {undefined, _, _} -> trace_back_zero_or_one(Outs, Index, Index,   Acc);
        {_, undefined, _} -> trace_back_zero_or_one(Outs, Index, Index+1, Acc);
        {_, _, undefined} -> trace_back_zero_or_one(Outs, Index, Index+2, Acc);
        {_, _, _        } ->
            Digit1I = utils:bit_invert(Digit1),
            Shift = utils:bits_to_integer([Digit1I, Digit2, Digit3]),
            C1 = maps:get(Index+Shift,   Acc, undefined),
            C2 = maps:get(Index+Shift+1, Acc, undefined),
            C3 = maps:get(Index+Shift+2, Acc, undefined),
            case {C1, C2, C3} of
                {undefined, _, _} -> trace_back_zero_or_one(Outs, Index, Index+Shift,   Acc);
                {_, undefined, _} -> trace_back_zero_or_one(Outs, Index, Index+Shift+1, Acc);
                {_, _, undefined} -> trace_back_zero_or_one(Outs, Index, Index+Shift+2, Acc);
                {_, _, _        } ->
                    Result1 =                  utils:bit_xor(Digit1I, C1),
                    Result2 = utils:bit_invert(utils:bit_xor(Digit2,  C2)),
                    Result3 = utils:bit_invert(utils:bit_xor(Digit3,  C3)),
                    Result = utils:bits_to_integer([Result1, Result2, Result3]),
                    %utils:print("~p~p~p ~p~p~p ~p~p~p", [Digit3, Digit2, Digit1I, C3, C2, C1, Result3, Result2, Result1]),
                    case Result =:= Out of
                        true  -> trace_back(OtherOuts, Index+3, Acc);
                        false -> undefined
                    end
            end
    end.


map_to_integer(Map) ->
    MaxIndex = lists:max(maps:keys(Map)),
    Bits = lists:map(fun(Index) ->
        case maps:get(Index, Map, undefined) of
            undefined -> 0;
            Digit     -> Digit
        end
    end, lists:seq(0, MaxIndex)),
    utils:bits_to_integer(Bits).


solve_1(FileName) ->
    {A, B, C, ProgramList} = read_inputs(FileName),
    Program = program_to_map(ProgramList, 0, #{}),
    %utils:print("A=~p, B=~p, C=~p, Program=~p", [A, B, C, Program]),
    {_, OutputR} = run(A, B, C, 0, Program, []),
    lists:append(lists:join(",",lists:map(fun erlang:integer_to_list/1, lists:reverse(OutputR)))).


solve_2(FileName) ->
    {_A, B, C, ProgramList} = read_inputs(FileName),
    Result = trace_back(ProgramList, 0, #{}),
    Int = map_to_integer(Result),
    % Result check
    Program = program_to_map(ProgramList, 0, #{}),
    {[], ProgramListR} = run(utils:integer_to_bits(Int), B, C, 0, Program, []),
    ProgramList = lists:reverse(ProgramListR),
    Int.
