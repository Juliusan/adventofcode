-module(day07).
-export([solve_1/1, solve_2/1]).


% Nebuvo sunku. Tik kokių 10 minučių niekaip nesupratu, kodėl pavyzdyje
% spliterių yra 22, o rašo, kad spindulys buvo išskaidytas 21 kartą. Po to,
% žinoma, supratau, kad ne visi jie suveikia. Iš esmės antrai daliai kažko
% labai mandro nereikėjo. Tiko ir kiek modifikuota pirmos dalies programa.
% Tik vėliau jas apjungiau į vieną.


% (aoc_2025@JuliusErisataT14.erisata.lt)1> day07:solve_1("priv/day07-PVZ.txt").
% 21
% (aoc_2025@JuliusErisataT14.erisata.lt)2> day07:solve_1("priv/day07.txt").
% 1566
% (aoc_2025@JuliusErisataT14.erisata.lt)3> day07:solve_2("priv/day07-PVZ.txt").
% 40
% (aoc_2025@JuliusErisataT14.erisata.lt)4> day07:solve_2("priv/day07.txt").
% 5921061943075
% (aoc_2025@JuliusErisataT14.erisata.lt)5> timer:tc(fun() -> day07:solve_1("priv/day07.txt") end).
% {33397,1566}
% (aoc_2025@JuliusErisataT14.erisata.lt)6> timer:tc(fun() -> day07:solve_2("priv/day07.txt") end).
% {30446,5921061943075}


join_beams(Beams) -> join_beams(Beams, []).

join_beams([], AccBeams) ->
    AccBeams;
join_beams([{Beam,PathCount1}, {Beam, PathCount2} | RemBeams], AccBeams) ->
    join_beams(RemBeams, [{Beam, PathCount1+PathCount2}|AccBeams]);
join_beams([BeamPathCount|RemBeams], AccBeams) ->
    join_beams(RemBeams, [BeamPathCount|AccBeams]).


solve(FileName) ->
    Lines = ja_erl_utils_file:read_lines_no_new_line(FileName),
    Map = ja_erl_utils_matrix:get_char_matrix(lists:reverse(Lines)),
    Start = ja_erl_utils_matrix:index_of($S, Map),
    %ja_erl_utils_terminal:print("~p", [Start]),
    {Rows, _Columns} = ja_erl_utils_matrix:dimensions(Map),
    lists:foldl(fun(Row, {Beams, Splits}) ->
        {NewBeams, NewSplits} = lists:foldl(fun({{_, Column}, PathCount}, {AccBeams, AccSplits}) ->
            Beam = {Row, Column},
            case ja_erl_utils_matrix:get(Beam, Map) of
                $. ->
                    {[{Beam, PathCount} | AccBeams], AccSplits};
                $^ ->
                    NewBeams = utils:ja_erl_utils_matrix_get_indices(Beam, [left, right], Map),
                    NewBeamPathCounts = lists:map(fun(B) -> {B, PathCount} end, NewBeams),
                    NewAccBeams = NewBeamPathCounts ++ AccBeams,
                    {NewAccBeams, AccSplits + 1}
                end
        end, {[], Splits}, Beams),
        NewBeamsJoined = join_beams(NewBeams),
        %ja_erl_utils_terminal:print("~p", [NewBeamsJoined]),
        {NewBeamsJoined, NewSplits}
    end, {[{Start, 1}], 0}, lists:seq(2, Rows)).


solve_1(FileName) ->
    {_Beams, Splits} = solve(FileName),
    Splits.


solve_2(FileName) ->
    {Beams, _Splits} = solve(FileName),
    ja_erl_utils_list:map_sum(fun({_, PathCount}) -> PathCount end, Beams).
