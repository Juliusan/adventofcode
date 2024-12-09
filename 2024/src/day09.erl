-module(day09).
-export([solve_1/1, solve_2/1]).

% Kažkaip jau darosi tradicija: lyg ir viskas aišku, sugalvoji algoritmą ir
% tada prasėdi jį implementuodamas. Galvojau, bent antra dalis greitai gausis,
% bet iš esmės parašiau atskirą algoritmą tam. Na ir smulkmenų gaudymas užtrunka.
% Antros dalies veikimo greitis - 2.6-2.7s. Pakenčiamai. Dar turiu minčių
% tobulinimui. Įdomu, ar turėsiu laiko.

% (aoc_2024@JuliusErisataT14.erisata.lt)1> day09:solve_1("priv/day09-PVZ1.txt").
% 60
% (aoc_2024@JuliusErisataT14.erisata.lt)2> day09:solve_1("priv/day09-PVZ2.txt").
% 1928
% (aoc_2024@JuliusErisataT14.erisata.lt)3> day09:solve_1("priv/day09.txt").
% 6334655979668
% (aoc_2024@JuliusErisataT14.erisata.lt)4> day09:solve_2("priv/day09-PVZ2.txt").
% 2858
% (aoc_2024@JuliusErisataT14.erisata.lt)5> day09:solve_2("priv/day09.txt").
% 6349492251099


read_line(Line) ->
    read_line(Line, true, 0, []).

read_line([], _, _, Acc) ->
    lists:reverse(Acc);

read_line([LChar|Line], IsFile, FileId, Acc) ->
    Length = LChar - $0,
    case IsFile of
        true  -> read_line(Line, not(IsFile), FileId+1, [{FileId, Length}|Acc]);
        false -> read_line(Line, not(IsFile), FileId,   [{empty,  Length}|Acc])
    end.

read_inputs(FileName) ->
    utils:read_lines_no_new_line_to_elems(FileName, fun read_line/1).


print_file_list([])                                              -> io:fwrite("~n");
print_file_list([{empty,  Length}|List])                         -> io:fwrite(lists:duplicate(Length, $.                   )), print_file_list(List);
print_file_list([{FileId, Length}|List]) when FileId rem 36 < 10 -> io:fwrite(lists:duplicate(Length, (FileId rem 36)+$0   )), print_file_list(List);
print_file_list([{FileId, Length}|List])                         -> io:fwrite(lists:duplicate(Length, (FileId rem 36)-10+$A)), print_file_list(List).


compact_file_list(FileList) ->
    Files = lists:reverse(lists:filter(fun
        ({empty, _}) -> false;
        ({_,     _}) -> true
    end, FileList)),
    compact_file_list(FileList, Files, -1, [], 0).


compact_file_list([{empty, LengthE}|FileList], [{FileId, LengthF}|Files], LastFileId, AccList, AccEmpty) when FileId > LastFileId ->
    %utils:print("XXX 1 ~p ~p ~p", [{empty, LengthE}, {FileId, LengthF}, AccEmpty]),
    case {LengthE, LengthF} of
        {LE, LF} when LE  >  LF ->
            compact_file_list([{empty, LengthE-LengthF}|FileList], Files, LastFileId, [{FileId,LengthF}|AccList], AccEmpty+LengthF);
        {LE, LF} when LE =:= LF ->
            compact_file_list(FileList, Files,LastFileId, [{FileId,LengthF}|AccList], AccEmpty+LengthF);
        {LE, LF} when LE  <  LF ->
            compact_file_list(FileList, [{FileId, LengthF-LengthE}|Files], LastFileId, [{FileId,LengthE}|AccList], AccEmpty+LengthE)
    end;

compact_file_list([{empty, _} = Empty|FileList], _, _, AccList, AccEmpty) ->
    LengthE = lists:sum(lists:filtermap(fun
        ({empty, Length}) -> {true, Length};
        ({_,     _     }) -> false
    end, [Empty|FileList])),
    %utils:print("XXX 2 ~p ? ~p", [{empty, LengthE}, AccEmpty]),
    lists:reverse([{empty, LengthE+AccEmpty}|AccList]);

compact_file_list([{FileId, _}|FileList], [{FileId, LengthF}|Files], _, AccList, AccEmpty) ->
    %utils:print("XXX 5 ? ~p ~p", [{FileId, LengthF}, AccEmpty]),
    compact_file_list(FileList, Files, FileId, [{FileId, LengthF}|AccList], AccEmpty);

compact_file_list([{FileId, LengthE}|FileList], Files, _, AccList, AccEmpty) ->
    %utils:print("XXX 3 ~p ? ~p", [{FileId, LengthE}, AccEmpty]),
    compact_file_list(FileList, Files, FileId, [{FileId, LengthE}|AccList], AccEmpty).


split_file_list([], _, AccFiles, AccEmpty) ->
    {lists:reverse(AccEmpty), AccFiles};

split_file_list([{empty, Length}|FileList], Index, AccFiles, AccEmpty) ->
    split_file_list(FileList, Index+Length, AccFiles, [{Index, empty, Length}|AccEmpty]);

split_file_list([{FileId, Length}|FileList], Index, AccFiles, AccEmpty) ->
    split_file_list(FileList, Index+Length, [{Index, FileId, Length}|AccFiles], AccEmpty).


compact_file_list2(FileList) ->
    {EmptyList, FileOnlyList} = split_file_list(FileList, 0, [], []),
    FilterZerosFun = fun({_,_,0}) -> false; (_) -> true end,
    EmptyListF    = lists:filter(FilterZerosFun, EmptyList),
    FileOnlyListF = lists:filter(FilterZerosFun, FileOnlyList),
    %utils:print("SPLIT: ~p ~p", [FileOnlyListF, EmptyListF]),
    compact_file_list2(FileOnlyListF, EmptyListF, []).

compact_file_list2([], EmptyList, AccFileList) ->
    Result = lists:sort(AccFileList ++ EmptyList),
    lists:map(fun({_, Type, Length}) -> {Type, Length} end, Result);

compact_file_list2([File|FileList], EmptyList, Acc) ->
    {NewFile, NewEmptyList} = put_file(File, EmptyList, []),
    NewEmptyListC = compact_list(NewEmptyList, []),
    compact_file_list2(FileList, NewEmptyListC, [NewFile|Acc]).


compact_list([], Acc) -> lists:reverse(Acc);
compact_list([Elem], Acc) -> lists:reverse([Elem|Acc]);
compact_list([{Index1, empty, Length1},{Index2, empty, Length2}|List], Acc) when Index1+Length1 =:= Index2 ->
    compact_list([{Index1, empty, Length1+Length2}|List], Acc);
compact_list([Elem|List], Acc) ->
    compact_list(List, [Elem|Acc]).


put_file(File, [], Acc) -> {File, lists:reverse(Acc)};
put_file({IndexF, FileId, LengthF}, [{IndexE, empty, LengthE}|EmptyList], Acc) when IndexE < IndexF, LengthF < LengthE ->
    {{IndexE, FileId, LengthF}, lists:reverse(Acc) ++ lists:sort([{IndexE+LengthF, empty, LengthE-LengthF},{IndexF, empty, LengthF}|EmptyList])};
put_file({IndexF, FileId, LengthF}, [{IndexE, empty, LengthE}|EmptyList], Acc) when IndexE < IndexF, LengthF =:= LengthE ->
    {{IndexE, FileId, LengthF}, lists:reverse(Acc) ++ lists:sort([{IndexF, empty, LengthE}|EmptyList])};
put_file({IndexF, FileId, LengthF}, [{IndexE, empty, LengthE}|EmptyList], Acc) when IndexE < IndexF, LengthF > LengthE ->
    put_file({IndexF, FileId, LengthF}, EmptyList, [{IndexE, empty, LengthE}|Acc]);
put_file({IndexF, FileId, LengthF}, [{IndexE, empty, LengthE}|EmptyList], Acc) when IndexE > IndexF ->
    {{IndexF, FileId, LengthF}, lists:reverse(Acc)++[{IndexE, empty, LengthE}|EmptyList]}.


count_result(FileList) ->
    count_result(FileList, 0, 0).

count_result([], _, Acc) -> Acc;
count_result([{empty, Length}|FileList], Index, Acc) -> count_result(FileList, Index+Length, Acc);
count_result([{FileId, Length}|FileList], Index, Acc) when is_integer(FileId) ->
    Result = (FileId*Length*(2*Index+Length-1)) div 2,
    % utils:print("XXX ~p ~p ~p ~p", [FileId, Length, Index, Result]),
    count_result(FileList, Index+Length, Acc+Result).


solve_1(FileName) ->
    [FileList] = read_inputs(FileName),
    %print_file_list(FileList),
    FL2 = compact_file_list(FileList),
    %print_file_list(FL2),
    count_result(FL2).


solve_2(FileName) ->
    [FileList] = read_inputs(FileName),
    %print_file_list(FileList),
    FL2 = compact_file_list2(FileList),
    %print_file_list(FL2),
    count_result(FL2).
