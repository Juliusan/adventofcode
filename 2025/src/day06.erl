-module(day06).
-export([solve_1/1, solve_2/1]).


% Tai štai kur pravertė apversto eilučių sąrašo grąžinimas! Gavosi taip, kad
% veiksmai atsidūrė pirmoje eilutėje, o ne paskutinėje. Visa kita buvo
% nesudėtinga ir aišku: transponavimas, pasipildymas tarpais priekyje. Užtruko
% tik kodo surašymas. Visi atsakymai teisingi gavosi pirmuoju bandymu.

% (aoc_2025@JuliusErisataT14.erisata.lt)1> day06:solve_1("priv/day06-PVZ.txt").
% 4277556
% (aoc_2025@JuliusErisataT14.erisata.lt)2> day06:solve_1("priv/day06.txt").
% 6171290547579
% (aoc_2025@JuliusErisataT14.erisata.lt)3> day06:solve_2("priv/day06-PVZ.txt").
% 3263827
% (aoc_2025@JuliusErisataT14.erisata.lt)4> day06:solve_2("priv/day06.txt").
% 8811937976367
% (aoc_2025@JuliusErisataT14.erisata.lt)5> timer:tc(fun() -> day06:solve_1("priv/day06.txt") end).
% {5245,6171290547579}
% (aoc_2025@JuliusErisataT14.erisata.lt)6> timer:tc(fun() -> day06:solve_2("priv/day06.txt") end).
% {10968,8811937976367}


solve_1(FileName) ->
    Lines = ja_erl_utils_file:read_lines_no_new_line_to_elems(FileName, fun(Line) ->
        Split = string:split(Line, " ", all),
        lists:filter(fun(Elem) -> Elem =/= "" end, Split)
    end),
    %ja_erl_utils_terminal:print("~p", [Lines]),
    Problems = ja_erl_utils_list:transpose(Lines),
    %ja_erl_utils_terminal:print("~p", [Problems]),
    ja_erl_utils_list:map_sum(fun(Problem) ->
        [Action | Numbers] = Problem,
        case Action of
            "+" -> ja_erl_utils_list:map_sum(fun(Number) -> erlang:list_to_integer(Number) end, Numbers);
            "*" -> lists:foldl(fun(Number, Acc) -> erlang:list_to_integer(Number)*Acc end, 1, Numbers)
        end
    end, Problems).


solve_2(FileName) ->
    Lines = ja_erl_utils_file:read_lines_no_new_line_to_elems(FileName, fun lists:reverse/1),
    LongestLine = lists:foldl(fun(Line, Acc) ->
        Length = erlang:length(Line),
        erlang:max(Acc, Length)
    end, 0, Lines),
    EqualLines = lists:map(fun(Line) ->
        string:right(Line, LongestLine, $ )
    end, Lines),
    %ja_erl_utils_terminal:print("~p", [EqualLines]),
    Columns = ja_erl_utils_list:transpose(EqualLines),
    %ja_erl_utils_terminal:print("~p", [Columns]),
    {Result, []} = ja_erl_utils_list:map_sum_foldl(fun([Operation| NumberStr], AccNumbers) ->
        case string:trim(NumberStr) of
            "" ->
                AccNumbers = [], % Just an assertion
                {0, []};
            NumberTrimmed ->
                Number = erlang:list_to_integer(lists:reverse(NumberTrimmed)),
                case Operation of
                    $  -> {0, [Number | AccNumbers]};
                    $+ -> {lists:sum([Number|AccNumbers]), []};
                    $* -> {lists:foldl(fun(N, Acc) -> N*Acc end, 1, [Number|AccNumbers]), []}
                end
        end
    end, [], Columns),
    Result.
