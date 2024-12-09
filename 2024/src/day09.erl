-module(day09).
-export([solve_1/1, solve_2/1]).

% Kažkaip jau darosi tradicija: lyg ir viskas aišku, sugalvoji algoritmą ir
% tada prasėdi jį implementuodamas. Galvojau, bent antra dalis greitai gausis,
% bet iš esmės parašiau atskirą algoritmą tam. Na ir smulkmenų gaudymas užtrunka.
% Antros dalies veikimo greitis - 2.6-2.7s. Pakenčiamai. Dar turiu minčių
% tobulinimui. Įdomu, ar turėsiu laiko. Turėjau. Abi dalis skaičiuoja iki 60 ms.
% Vieną sykį netgi mačiau 22 ms.

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


read_disk_map(FileName) ->
    Line = utils:read_only_line_no_new_line(FileName),
    read_line(Line, true, 0, 0, []).

read_line([], _, _, _, Acc) ->
    lists:reverse(Acc);

read_line([LChar|Line], IsFile, Index, FileId, Acc) ->
    Length = LChar - $0,
    case {Length, IsFile} of
        {0, true } -> read_line(Line, false, Index,        FileId+1, Acc);
        {0, false} -> read_line(Line, true,  Index,        FileId,   Acc);
        {_, true } -> read_line(Line, false, Index+Length, FileId+1, [{Index, FileId, Length}|Acc]);
        {_, false} -> read_line(Line, true,  Index+Length, FileId,   [{Index, empty,  Length}|Acc])
    end.


print_file_list([])                                                 -> io:fwrite("~n");
print_file_list([{_, empty,  Length}|List])                         -> io:fwrite(lists:duplicate(Length, $.                   )), print_file_list(List);
print_file_list([{_, FileId, Length}|List]) when FileId rem 36 < 10 -> io:fwrite(lists:duplicate(Length, (FileId rem 36)+$0   )), print_file_list(List);
print_file_list([{_, FileId, Length}|List])                         -> io:fwrite(lists:duplicate(Length, (FileId rem 36)-10+$A)), print_file_list(List).


split_disk_map([],                                 AccFiles, AccEmpty) -> {AccFiles, lists:reverse(AccEmpty)};
split_disk_map([{_, empty,  _} = Empty | DiskMap], AccFiles, AccEmpty) -> split_disk_map(DiskMap, AccFiles,        [Empty|AccEmpty]);
split_disk_map([{_, _,      _} = File  | DiskMap], AccFiles, AccEmpty) -> split_disk_map(DiskMap, [File|AccFiles], AccEmpty        ).


merge_disk_map([], EmptyList, Acc) -> lists:reverse(Acc) ++ EmptyList;
merge_disk_map(FileList, [], Acc) -> lists:reverse(Acc) ++ FileList;
merge_disk_map([{IndexF,_,_}=File | FileList], [{IndexE,_,_}=Empty|EmptyList], Acc) when IndexF < IndexE -> merge_disk_map(FileList, [Empty|EmptyList], [File|Acc]);
merge_disk_map([{IndexF,_,_}=File | FileList], [{IndexE,_,_}=Empty|EmptyList], Acc) when IndexF > IndexE -> merge_disk_map([File|FileList], EmptyList,  [Empty|Acc]).


defragment(DiskMap, Part) ->
    {FileList, EmptyList} = split_disk_map(DiskMap, [], []),
    %utils:print("SPLIT: ~p ~p", [FileOnlyListF, EmptyListF]),
    {NewFileList, NewEmptyList} = case Part of
        1 -> defragment_1(FileList, EmptyList, [], []);
        2 -> defragment_2(FileList, EmptyList, [], [])
    end,
    merge_disk_map(NewFileList, NewEmptyList, []).


defragment_1([], EmptyList, AccFileList, AccEmptyList) ->
    {lists:reverse(AccFileList), merge_disk_map(EmptyList, AccEmptyList, [])};

defragment_1([{IF, FileId, LF}|FileList], [{IE, empty, LE}|EmptyList], AccFileList, AccEmptyList) when IE < IF, LF < LE ->
    defragment_1(FileList, [{IE+LF, empty, LE-LF}|EmptyList], [{IE, FileId, LF}|AccFileList], [{IF, empty, LF}|AccEmptyList]);

defragment_1([{IF, FileId, LF}|FileList], [{IE, empty, LE}|EmptyList], AccFileList, AccEmptyList) when IE < IF, LF =:= LE ->
    defragment_1(FileList, EmptyList, [{IE, FileId, LF}|AccFileList], [{IF, empty, LF}|AccEmptyList]);

defragment_1([{IF, FileId, LF}|FileList], [{IE, empty, LE}|EmptyList], AccFileList, AccEmptyList) when IE < IF, LF > LE ->
    defragment_1([{IF, FileId, LF-LE}|FileList], EmptyList, [{IE, FileId, LE}|AccFileList], [{IF+LE, empty, LE}|AccEmptyList]);

defragment_1([{IF, FileId, LF}|FileList], [{IE, empty, IL}|EmptyList], AccFileList, AccEmptyList) when IE > IF ->
    FileListM = merge_disk_map(lists:reverse([{IF, FileId, LF}|FileList]), lists:reverse(AccFileList), []),
    EmptyListM = merge_disk_map([{IE, empty, IL}|EmptyList], AccEmptyList, []),
    {FileListM, EmptyListM}.


defragment_2([], EmptyList, AccFileList, AccEmptyList) ->
    {lists:sort(AccFileList), merge_disk_map(EmptyList, AccEmptyList, [])};

defragment_2([File|FileList], EmptyList, AccFileList, AccEmptyList) ->
    {{IndexF, _, LengthF} = NewFile, Action} = defragment_file_2(File, EmptyList),
    {NewEmptyList, NewAccEmptyList} = case Action of
        none ->
            {EmptyList, AccEmptyList};
        {replace, LengthE, IndexNE} ->
            NEL = lists:keyreplace(IndexF, 1, EmptyList, {IndexF+LengthF, empty, LengthE}),
            NAEL = [{IndexNE, empty, LengthF}|AccEmptyList],
            {NEL, NAEL};
        {drop, IndexNE} ->
            NEL = lists:keydelete(IndexF, 1, EmptyList),
            NAEL = [{IndexNE, empty, LengthF}|AccEmptyList],
            {NEL, NAEL}
    end,
    defragment_2(FileList, NewEmptyList, [NewFile|AccFileList], NewAccEmptyList).


defragment_file_2(File,             [])                                                  -> {File,             none};
defragment_file_2({IF, FileId, LF}, [{IE, empty, LE}|_        ]) when IE < IF, LF  <  LE -> {{IE, FileId, LF}, {replace, LE-LF, IF}};
defragment_file_2({IF, FileId, LF}, [{IE, empty, LE}|_        ]) when IE < IF, LF =:= LE -> {{IE, FileId, LF}, {drop, IF}};
defragment_file_2({IF, FileId, LF}, [{IE, empty, LE}|EmptyList]) when IE < IF, LF  >  LE -> defragment_file_2({IF, FileId, LF}, EmptyList);
defragment_file_2({IF, FileId, LF}, [{IE, empty, _ }|_        ]) when IE > IF            -> {{IF, FileId, LF}, none}.


count_result(FileList) ->
    count_result(FileList, 0, 0).

count_result([], _, Acc) -> Acc;
count_result([{_, empty, Length}|FileList], Index, Acc) -> count_result(FileList, Index+Length, Acc);
count_result([{_, FileId, Length}|FileList], Index, Acc) when is_integer(FileId) ->
    Result = (FileId*Length*(2*Index+Length-1)) div 2,
    count_result(FileList, Index+Length, Acc+Result).


solve(FileName, Part) ->
    DiskMap = read_disk_map(FileName),
    %print_file_list(DiskMap),
    DiskMapD = defragment(DiskMap, Part),
    %print_file_list(DiskMapD),
    count_result(DiskMapD).

solve_1(FileName) ->
    solve(FileName, 1).


solve_2(FileName) ->
    solve(FileName, 2).
