-module(day02).
-export([solve_1/1, solve_2/1]).

% Užduotis nesunki. Iš karto sugalvojau skaičius pjauti per pusę ir nuo-iki
% daryti tik pirmajai pusei. Tiesa, užtruko, kol visą tai tvarkingai surašiau.
% Antrojoje dalyje irgi toks mąstymas padėjo: ieškoti pasikartojančios dalies
% reikėjo tik iki skaičiaus vidurio. Tiesa, užtruko, kol susidėlioja tvarkingą
% pasikartojimų istoriją. Įdomu, kad pirmuoju pandymu gavau per didelį
% rezultatą, bet benagrinėdamas, kokie skaičiai buvo sudėti, pastebėjau, kad
% 1 yra tarp jų. Neturėdamas laiko greitai taisyti kodo, pabandžiau 1 mažesnį
% atsakymą ir pavyko. Tik vėliau prirašiau padorų kodo pataisymą.

% (aoc_2025@JuliusErisataT14.erisata.lt)1> day02:solve_1("priv/day02-PVZ.txt").
% 1227775554
% (aoc_2025@JuliusErisataT14.erisata.lt)2> day02:solve_1("priv/day02.txt").
% 8576933996
% (aoc_2025@JuliusErisataT14.erisata.lt)3> day02:solve_2("priv/day02-PVZ.txt").
% 4174379265
% (aoc_2025@JuliusErisataT14.erisata.lt)4> day02:solve_2("priv/day02.txt").
% 25663320831
% (aoc_2025@JuliusErisataT14.erisata.lt)5> timer:tc(fun() -> day02:solve_1("priv/day02.txt") end).
% {356,8576933996}
% (aoc_2025@JuliusErisataT14.erisata.lt)6> timer:tc(fun() -> day02:solve_2("priv/day02.txt") end).
% {123472,25663320831}


read(FileName) ->
    Line = ja_erl_utils_file:read_only_line_no_new_line(FileName),
    Ranges = string:split(Line, ",", all),
    lists:map(fun(Range) -> string:split(Range, "-") end, Ranges).


check_pattern(_Pattern, 1, _FromInt, _ToInt, UsedNumbers) ->
    {0, UsedNumbers};

check_pattern(Pattern, Repeats, FromInt, ToInt, UsedNumbers) ->
    Number = erlang:list_to_integer(lists:append(lists:duplicate(Repeats, Pattern))),
    case FromInt =< Number andalso Number =< ToInt of
        true  ->
            case lists:member(Number, UsedNumbers) of
                false -> {Number, [Number | UsedNumbers]};
                true  -> {0, UsedNumbers}
            end;
        false ->
            {0, UsedNumbers}
    end.


solve_1(FileName) ->
    Ranges = read(FileName),
    %ja_erl_utils_terminal:print("~p", [Ranges]),
    ja_erl_utils_list:map_sum(fun([From, To]) ->
        %ja_erl_utils_terminal:print("~p - ~p", [string:len(From), string:len(To)]),
        FromLen = string:len(From),
        FromH = case FromLen rem 2 of
            0 -> string:left(From, FromLen div 2);
            1 -> string:left("1", FromLen div 2 + 1, $0)
        end,
        ToLen = string:len(To),
        ToH = case ToLen rem 2 of
            0 -> string:left(To, ToLen div 2);
            1 -> string:left("", ToLen div 2, $9)
        end,
        FromInt = erlang:list_to_integer(From),
        ToInt = erlang:list_to_integer(To),
        %ja_erl_utils_terminal:print("~s-~s -> ~s-~s -> ", [From, To, FromH, ToH]),
        Sum = ja_erl_utils_list:map_sum(fun(Number) ->
            NumberStr = erlang:integer_to_list(Number),
            NumberNumber = erlang:list_to_integer(NumberStr ++ NumberStr),
            %ja_erl_utils_terminal:print("\t~p", [NumberNumber]),
            case FromInt =< NumberNumber andalso NumberNumber =< ToInt of
                true  -> NumberNumber;
                false -> 0
            end
        end, lists:seq(erlang:list_to_integer(FromH), erlang:list_to_integer(ToH))),
        %ja_erl_utils_terminal:print("~s-~s -> ~s-~s -> ~p", [From, To, FromH, ToH, Sum]),
        Sum
    end, Ranges).


solve_2(FileName) ->
    Ranges = read(FileName),
    % lists:foreach(fun([From, To]) ->
    %     ja_erl_utils_terminal:print("~p: ~s-~s", [erlang:list_to_integer(To) - erlang:list_to_integer(From), From, To])
    % end, Ranges),
    ja_erl_utils_list:map_sum(fun([From, To]) ->
        %ja_erl_utils_terminal:print("~p - ~p", [string:len(From), string:len(To)]),
        FromLen = string:len(From),
        FromH = case string:left(From, FromLen div 2) of
            "" -> "1";
            H  -> H
        end,
        ToLen = string:len(To),
        ToH = string:left(To, ToLen div 2 + ToLen rem 2),
        FromInt = erlang:list_to_integer(From),
        ToInt = erlang:list_to_integer(To),
        %ja_erl_utils_terminal:print("~s-~s -> ~s-~s -> ", [From, To, FromH, ToH]),
        {Sum, _FinalFoundNumbers} = ja_erl_utils_list:map_sum_foldl(fun(Number, FoundNumbers) ->
            NumberStr = erlang:integer_to_list(Number),
            ja_erl_utils_list:map_sum_foldl(fun(DigitCount, FoundNumbersIn) ->
                Pattern = string:left(NumberStr, DigitCount),
                {FromNumber, FoundNumbersOut1} = case FromLen rem DigitCount of
                    0 -> check_pattern(Pattern, FromLen div DigitCount, FromInt, ToInt, FoundNumbersIn);
                    _ -> {0, FoundNumbersIn}
                end,
                {ToNumber, FoundNumbersOut2} = case ToLen rem DigitCount of
                    0 -> check_pattern(Pattern, ToLen div DigitCount, FromInt, ToInt, FoundNumbersOut1);
                    _ -> {0, FoundNumbersOut1}
                end,
                {FromNumber + ToNumber, FoundNumbersOut2}
            end, FoundNumbers, lists:seq(1, string:len(NumberStr)))
        end, [], lists:seq(erlang:list_to_integer(FromH), erlang:list_to_integer(ToH))),
        %ja_erl_utils_terminal:print("\t~p", [lists:usort(FinalFoundNumbers)]),
        Sum
    end, Ranges).
