-module(utils).
-export([print/2, print_char_matrix/2]).
-export([wait_key_press/0]).
-export([
    read_file/1,
    read_only_line/1,
    read_only_line_no_new_line/1,
    read_lines/1,
    read_lines_no_new_line/1,
    read_lines_to_elems/2,
    read_lines_to_elems/3,
    read_lines_no_new_line_to_elems/2,
    read_lines_no_new_line_to_elems/3
]).
-export([get_integer/1, get_integer_list/1, get_integer_list/2, get_new_matrix/2, get_char_matrix/1]).
-export([drop_trailing_new_line/1]).
-export([count_elems_sorted/2, count_elems_start/1]).
-export([is_decreasing/2, is_increasing/2]).
-export([
    transpose/1,
    diagonals_f/1,
    diagonals_b/1,
    middle/1,
    middle_single/1,
    intersection/2,
    foldl_pairs/3,
    list_map_sum/2,
    list_filter_count/2,
    list_foldl_sum/3,
    list_foldl_count/3
]).
-export([map_reverse/1, map_max_value/1, map_min_value/1, map_map_sum/2, map_map_count/2]).
-export([matrix_index_of/2, matrix_is_valid_index/2, matrix_next_index/3, matrix_foldl/4]).
-export([direction_all/0, direction_reverse/1, direction_clockwise/1, direction_counterclockwise/1]).
-export([integer_digit_count/1, integer_10_pow/1, concat_integers/2, split_integer/2]).
-export([euclidean_div/2, euclidean_rem/2]).
-export([solve_one_equation_int/1, solve_two_equations_int/2]).
-export([integer_to_bits/1, bits_to_integer/1, bit_and/2, bit_or/2, bit_xor/2, bits_and/2, bits_or/2, bits_xor/2, bit_invert/1]).


-type matrix(Type) :: #{matrix_index() => Type}.
-type matrix_index() :: {Row :: integer(), Column :: integer()}.
-type matrix_direction() :: up | right | down | left.
-type bit() :: 0 | 1.


-define(CUT_INTEGER,       1_000_000_000_000).
-define(CUT_INTEGER_POWER, 12).
-define(IS_BIT(B), (B =:= 0 orelse B =:= 1)).


%%
%%  Outputs message to terminal.
%%
-spec print(
    Pattern :: string(),
    Params  :: [term()]
) ->
    ok.

print(Pattern, Params) -> io:fwrite(Pattern ++ "~n", Params).


%%
%%  Outputs matrix Matrix, which has Dimensions dimensions (rows and columns) to terminal.
%%
-spec print_char_matrix(
    Matrix     :: matrix(term()),
    Dimensions :: matrix_index()
) ->
    ok.

print_char_matrix(Matrix, {Rows, Cols}) ->
    lists:foreach(fun(Row) ->
        lists:foreach(fun(Col) ->
            io:fwrite("~s", [[maps:get({Row, Col}, Matrix)]])
        end, lists:seq(1, Cols)),
        io:fwrite("~n")
    end, lists:seq(1, Rows)).


%%
%%  Waits for <enter> key press.
%%
-spec wait_key_press() -> ok.

wait_key_press() ->
    io:fread("", ""),
    ok.


%%
%%  Reads the whole file FileName into a single string.
%%
-spec read_file(
    FileName :: string()
) ->
    Contents :: string().

read_file(FileName) ->
    Lines = read_lines(FileName),
    LinesR = lists:reverse(Lines),
    lists:append(LinesR).


%%
%%  Reads file FileName and returns the single line of the file.
%%  Crashes if the file contains more than one line.
%%
-spec read_only_line(
    FileName :: string()
) ->
    Line :: string().

read_only_line(FileName) ->
    [Line] = read_lines(FileName),
    Line.


%%
%%  Reads file FileName and returns the single line of the file without new
%%  line in the end. Crashes if the file contains more than one line.
%%
-spec read_only_line_no_new_line(
    FileName :: string()
) ->
    Line :: string().

read_only_line_no_new_line(FileName) ->
    [Line] = read_lines_no_new_line(FileName),
    Line.


%%
%%  Reads file FileName and returns the list of lines.
%%  The returned list is in reversed order compared to input file.
%%
-spec read_lines(
    FileName :: string()
) ->
    [Line :: string()].

read_lines(FileName) -> read_lines_to_elems(FileName, fun(Line) -> Line end).


%%
%%  Reads file FileName and returns the list of lines without new line in the end.
%%  The returned list is in reversed order compared to input file.
%%
-spec read_lines_no_new_line(
    FileName :: string()
) ->
    [Line :: string()].

read_lines_no_new_line(FileName) -> read_lines_to_elems(FileName, fun drop_trailing_new_line/1).


%%
%%  Reads file FileName and each line converts to a single element in the resulting list, using LineToElemFun.
%%  The returned list is in reversed order compared to input file.
%%
-spec read_lines_to_elems(
    FileName      :: string(),
    LineToElemFun :: fun((Line :: string()) -> ElemType)
) ->
    [ElemType] when
        ElemType :: term().

read_lines_to_elems(FileName, LineToElemFun) ->
    [Elems] = read_lines_to_elems(FileName, [LineToElemFun], undefined),
    Elems.


%%
%%  Reads file FileName, which consists of several parts, separated by line Separator.
%%  Each line in each part is converted to a single element in the resulting list,
%%  using LineToElemFuns. Lines in first part is converted using first fun of LineToElemFuns,
%%  lines in second part - using second fun of LineToElemFuns etc. The list of parts
%%  is returned. Each part is a list of elements in reversed order compared to lines
%%  of input file, which were used to make them. However, the parts are in the same
%%  order as in input file.
%%
-spec read_lines_to_elems(
    FileName       :: string(),
    LineToElemFuns :: [fun((Line :: string()) -> ElemType)],
    Separator      :: string()
) ->
    [[ElemType]] when
        ElemType :: term().

read_lines_to_elems(FileName, LineToElemFuns, Separator) ->
    {ok, File} = file:open(FileName, [read]),
    Result = read_lines_to_elems(File, LineToElemFuns, Separator, [], []),
    ok = file:close(File),
    Result.

read_lines_to_elems(_, [], _, AccParts, []) ->
    lists:reverse(AccParts);

read_lines_to_elems(File, [LineToElemFun | LineToElemFuns], Separator, AccParts, AccElems) ->
    case file:read_line(File) of
        eof ->
            lists:reverse([AccElems | AccParts]);
        {ok, Separator} ->
            read_lines_to_elems(File, LineToElemFuns, Separator, [AccElems | AccParts], []);
        {ok, Line} ->
            Elem = LineToElemFun(Line),
            read_lines_to_elems(File, [LineToElemFun | LineToElemFuns], Separator, AccParts, [Elem | AccElems])
    end.


%%
%%  Same as read_lines_to_elems/2, but the read line is stripped of trailing
%%  new line character before passing to LineToElemFun.
%%
-spec read_lines_no_new_line_to_elems(
    FileName      :: string(),
    LineToElemFun :: fun((Line :: string()) -> ElemType)
) ->
    [ElemType] when
        ElemType :: term().

read_lines_no_new_line_to_elems(FileName, LineToElemFun) ->
    [Elems] = read_lines_no_new_line_to_elems(FileName, [LineToElemFun], undefined),
    Elems.


%%
%%  Same as read_lines_to_elems/3, but the read line is stripped of trailing
%%  new line character before passing any of LineToElemFun. Note that Separator
%%  is compared to the whole line, including trailing new line.
%%
-spec read_lines_no_new_line_to_elems(
    FileName       :: string(),
    LineToElemFuns :: [fun((Line :: string()) -> ElemType)],
    Separator      :: string()
) ->
    [[ElemType]] when
        ElemType :: term().

read_lines_no_new_line_to_elems(FileName, LineToElemFuns, Separator) ->
    NewLineToElemFuns = lists:map(fun(LineToElemFun) ->
        fun(Line) ->
            LineNoNL = drop_trailing_new_line(Line),
            LineToElemFun(LineNoNL)
        end
    end, LineToElemFuns),
    read_lines_to_elems(FileName, NewLineToElemFuns, Separator).


%%
%%  Reads an integer from the string until non digit character is encountered.
%%  Fails, if any other symbol is present. Returns remaining symbols in the line.
%%
-spec get_integer(string()) -> {integer(), Remaining :: string()}.

get_integer([$-|Line]) -> {Int, NewLine} = get_pos_integer(Line), {-Int, NewLine};
get_integer(Line)      ->  get_pos_integer(Line).

get_pos_integer(Line) ->
    case get_pos_integer(Line, 0) of
        {_, NewLine} = Result when Line =/= NewLine -> Result;
        _                                           -> throw("No digits found")
    end.

get_pos_integer([Digit|Else], Acc) when $0 =< Digit, Digit =< $9 -> get_pos_integer(Else, Acc*10 + Digit - $0);
get_pos_integer(Line,         Acc)                               -> {Acc, Line}.


%%
%%  Reads a list of space separated integers from Line string.
%%
-spec get_integer_list(string()) -> [integer()].

get_integer_list(Line) ->
    get_integer_list(Line, " ").


%%
%%  Reads a list of Separator separated integers from Line string.
%%
-spec get_integer_list(Line :: string(), Separator :: string()) -> [integer()].

get_integer_list("", _) ->
    [];

get_integer_list(Line, Separator) ->
    IntegerStrsOrEmpty = string:split(Line, Separator, all),
    IntegerStrs = lists:filter(fun("") -> false; (_) -> true end, IntegerStrsOrEmpty),
    lists:map(fun(IntegerStr) -> erlang:list_to_integer(IntegerStr) end, IntegerStrs).


%%
%%  Returns matrix with specified dimensions filled with Elem.
%%
-spec get_new_matrix(
        Elem       :: term(),
        Dimensions :: matrix_index()
    ) ->
        Matrix :: matrix(term()).

get_new_matrix(Element, {Rows, Cols}) ->
    lists:foldl(fun(Row, Acc) ->
        lists:foldl(fun(Col, Accc) ->
            Accc#{{Row, Col} => Element}
        end, Acc, lists:seq(1, Cols))
    end, #{}, lists:seq(1, Rows)).


%%
%%  Converts list of lines to a matrix, with each character as separate element.
%%  Line number in list ("row") is the first element of index, character's position
%%  in line ("column") is the second element. Total number of rows and columns (Dimensions)
%%  is also returned. It is expected (although not necessary for this function) that
%%  each matrix row contains the same number of elements.
%%
-spec get_char_matrix([Line :: string()]) ->
    {
        Matrix     :: matrix(char()),
        Dimensions :: matrix_index()
    }.

get_char_matrix([]) ->
    {#{}, {0, 0}};

get_char_matrix([FirstLine|_] = Lines) ->
    Cols = erlang:length(FirstLine),
    {Rows1, Matrix} = lists:foldl(fun(Line, {Row, Acc}) ->
        {Cols1, NewAcc} = lists:foldl(fun(Pos, {Col, Accc}) ->
            NewAccc = Accc#{{Row, Col} => Pos},
            {Col+1, NewAccc}
        end, {1, Acc}, Line),
        Cols = Cols1-1,
        {Row+1,NewAcc}
    end, {1, #{}}, Lines),
    {Matrix, {Rows1-1, Cols}}.


%%
%%  Drops trailing new line character from Line. It crashes, if Line does not
%%  end with new line.
%%
-spec drop_trailing_new_line(Line :: string()) -> string().

drop_trailing_new_line("\n")          -> "";
drop_trailing_new_line([Char | Line]) -> [Char | drop_trailing_new_line(Line)].


%%
%%  Counts number of elements Elem in sorted list List. Returns remaining list with all the Elems (and smaller elements) removed.
%%
-spec count_elems_sorted(Elem :: ElemType, List :: [ElemType]) ->
    {Count :: integer(), Remaining :: [ElemType]}
        when ElemType :: term().

count_elems_sorted(Elem, [E1|List]  ) when E1 < Elem -> count_elems_sorted(Elem, List);
count_elems_sorted(Elem, [Elem|List])                -> count_elems_start([Elem|List]);
count_elems_sorted(_,    List       )                -> {0, List}.


%%
%%  Counts how many times starting element of List is repeated at the start of the list.
%%  Returns remaining list with all the copies of starting element removed from the start.
%%
-spec count_elems_start(List :: [ElemType]) ->
    {Count :: integer(), Remaining :: [ElemType]}
        when ElemType :: term().

count_elems_start([Elem|List]) -> count_elems_start(Elem, List, 1).

count_elems_start(Elem, [Elem|List], Acc) -> count_elems_start(Elem, List, Acc+1);
count_elems_start(_,    List,        Acc) -> {Acc, List}.


%%
%%  Checks if List is in strictly descending order with consecutive elements not further
%%  apart than Diff
%%
-spec is_decreasing(List :: [number()], Diff :: number()) -> boolean().

is_decreasing([],                  _   ) -> true;
is_decreasing([_],                 _   ) -> true;
is_decreasing([Elem1, Elem2|Else], Diff) when Elem1 > Elem2, Elem1-Elem2 =< Diff -> is_decreasing([Elem2|Else], Diff);
is_decreasing(_,                   _   ) -> false.




%%
%%  Checks if List is in strictly ascending order with consecutive elements not further
%%  apart than Diff
%%
-spec is_increasing(List :: [number()], Diff :: number()) -> boolean().

is_increasing([], _) -> true;
is_increasing([_], _) -> true;
is_increasing([Elem1, Elem2|Else], Diff) when Elem2 > Elem1, Elem2-Elem1 =< Diff -> is_increasing([Elem2|Else], Diff);
is_increasing(_, _) -> false.


%%
%%  Transposes list of lists ListOfLists.
%%  This function is defined only if each list in ListOfLists is of the same size.
%%
-spec transpose(ListOfLists :: [List]) ->  ListOfLists :: [List]
    when List :: [term()].

transpose([])          -> [];
transpose([[]|_])      -> [];
transpose(ListOfLists) -> [lists:map(fun erlang:hd/1, ListOfLists) | transpose(lists:map(fun erlang:tl/1, ListOfLists))].


%%
%%  Returns all the forward diagonals (of the form /) of ListOfLists.
%%  Diagonals are returned in reverse order (starting from the bottom right corner and
%%  ending with the top left corned) and each one is reversed (from bottom left element
%%  to the top right one):
%%      diagonal_f(1 2) = [4], [3,2], [1]
%%                (3 4)
%%  This function is defined only if each list in ListOfLists is of the same size.
%%
-spec diagonals_f(ListOfLists :: [List]) ->  ListOfLists :: [List]
    when List :: [term()].


diagonals_f(ListOfLists) ->
    diagonals_f(ListOfLists, 1, []).

diagonals_f([],          _,     AccDiagonals) -> AccDiagonals;
diagonals_f(ListOfLists, Count, AccDiagonals) ->
    {Diagonal, NewListOfLists} = diagonal_f(ListOfLists, Count, [], []),
    diagonals_f(NewListOfLists, Count+1, [Diagonal|AccDiagonals]).

diagonal_f(ListOfLists,               0,     AccDiagonal, AccListOfLists) -> {AccDiagonal, lists:reverse(AccListOfLists)++ListOfLists};
diagonal_f([],                        _,     AccDiagonal, AccListOfLists) -> {AccDiagonal, lists:reverse(AccListOfLists)};
diagonal_f([[Elem]|ListOfLists],      Index, AccDiagonal, AccListOfLists) -> diagonal_f(ListOfLists, Index-1, [Elem|AccDiagonal], AccListOfLists);
diagonal_f([[Elem|List]|ListOfLists], Index, AccDiagonal, AccListOfLists) -> diagonal_f(ListOfLists, Index-1, [Elem|AccDiagonal], [List|AccListOfLists]).


%%
%%  Returns all the backwards diagonals (of the form \) of ListOfLists.
%%  Diagonals are returned starting from the bottom left corner and ending
%%  with the top right corned. Each of the diagonals is reversed (from bottom
%%  right element to the top left one):
%%      diagonal_f(1 2) = [3], [4,1], [2]
%%                (3 4)
%%  This function is defined only if each list in ListOfLists is of the same size.
%%
-spec diagonals_b(ListOfLists :: [List]) ->  ListOfLists :: [List]
    when List :: [term()].

diagonals_b(ListOfLists) ->
    ListOfListsT = lists:map(fun lists:reverse/1, ListOfLists),
    diagonals_f(ListOfListsT).


%%
%%  Returns the middle element(s) of the List. If the list contains odd number
%%  of elements, the returned list contains a single middle element. If however
%%  it contains even number of elements, two elements next to middle of the list
%%  is returned.
%%
-spec middle(List :: [ElemType]) -> [ElemType]
    when ElemType :: term().

middle(List) ->
    Length = erlang:length(List),
    Half = Length div 2,
    case Length rem 2  of
        0 -> [lists:nth(Half, List), lists:nth(Half+1, List) ];
        1 -> [lists:nth(Half + 1, List)]
    end.


%%
%%  Returns the single middle element of the List. Thus, this function is
%%  designed for lists containing odd number of elements. If List contains even
%%  number of elements, this function crashes.
%%
-spec middle_single(List :: [ElemType]) -> ElemType
    when ElemType :: term().

middle_single(List) ->
    [Middle] = middle(List),
    Middle.


%%
%%  Finds the intersection of two given lists. That is, returns a list,
%%  containing those elements, which are in both given lists.
%%
-spec intersection(
        List1 :: [ElemType],
        List2 :: [ElemType]
    ) ->
        Intersection :: [ElemType]
    when ElemType :: term().

intersection(List1, List2) ->
    [ Elem || Elem <- List1, lists:member(Elem, List2) ].


%%
%%  Folds through every pair of different elements of list List. If List is
%%  [1,2,3,...,n], then the fold order ir {1,2}, {1,3}, ..., {1,n}, {2,3}, ...,
%%  {2,n},...
%%
-spec foldl_pairs(
    FoldFun         :: fun((
                            Elem1       :: ElemType,
                            Elem2       :: ElemType,
                            Accumulator :: AccType
                        ) ->
                            NewAccumulator :: AccType
                    ),
    InitAccumulator :: AccType,
    List            :: [ElemType]
) ->
    FinalAccumulator :: AccType
        when
            ElemType :: term(),
            AccType  :: term().

foldl_pairs(_, AccIn, [])  -> AccIn;
foldl_pairs(_, AccIn, [_]) -> AccIn;
foldl_pairs(FoldFun, AccIn, [Elem|List]) ->
    AccOut = foldl_pairs(FoldFun, AccIn, Elem, List),
    foldl_pairs(FoldFun, AccOut, List).

foldl_pairs(_, AccIn, _, []) -> AccIn;
foldl_pairs(FoldFun, AccIn, Elem1, [Elem2|List]) ->
    AccOut = FoldFun(Elem1, Elem2, AccIn),
    foldl_pairs(FoldFun, AccOut, Elem1, List).


%%
%%  Maps every list element to number and adds those numbers.
%%
-spec list_map_sum(
    MapFun :: fun((Elem :: ElemType) -> MapResult :: number()),
    List   :: [ElemType]
) ->
    Sum :: number()
        when
            ElemType :: term().

list_map_sum(MapFun, List) ->
    lists:foldl(fun(Elem, Acc) ->
        Acc + MapFun(Elem)
    end, 0, List).


%%
%%  Filters list elements using FilterFun and counts the remaining ones.
%%
-spec list_filter_count(
    FilterFun :: fun((Elem :: ElemType) -> boolean()),
    List      :: [ElemType]
) ->
    Count :: number()
        when
            ElemType :: term().

list_filter_count(FilterFun, List) ->
    list_map_sum(fun(Elem) ->
        case FilterFun(Elem) of
            true  -> 1;
            false -> 0
        end
    end, List).


%%
%%  In addition to lists:foldl/3 maps every list element to number and adds
%%  those numbers.
%%
-spec list_foldl_sum(
    FoldFun         :: fun((
                            Elem        :: ElemType,
                            Accumulator :: AccType
                        ) -> {
                            MapResult      :: number(),
                            NewAccumulator :: AccType
                        }
                    ),
    InitAccumulator :: AccType,
    List            :: [ElemType]
) ->
    {Sum :: number(), FinalAccumulator :: AccType}
        when
            ElemType :: term(),
            AccType  :: term().

list_foldl_sum(FoldFun, InitAcc, List) ->
    lists:foldl(fun(Elem, {AccSum, Acc}) ->
        {MapResult, NewAcc} = FoldFun(Elem, Acc),
        {AccSum + MapResult, NewAcc}
    end, {0, InitAcc}, List).


%%
%%  In addition to lists:foldl/3 filters list elements using FoldFun and counts
%%  the remaining ones.
%%
-spec list_foldl_count(
    FoldFun         :: fun((
                            Elem        :: ElemType,
                            Accumulator :: AccType
                        ) -> {
                            CountElement   :: boolean(),
                            NewAccumulator :: AccType
                        }
                    ),
    InitAccumulator :: AccType,
    List            :: [ElemType]
) ->
    {Count :: number(), FinalAccumulator :: AccType}
        when
            ElemType :: term(),
            AccType  :: term().

list_foldl_count(FilterFun, InitAcc, List) ->
    list_foldl_sum(fun(Elem, Acc) ->
        case FilterFun(Elem, Acc) of
            {true,  NewAcc} -> {1, NewAcc};
            {false, NewAcc} -> {0, NewAcc}
        end
    end, InitAcc, List).


%%
%% Reverses the map su that instead of key-value pairs it stores value-key
%% pairs. This function expects that Map is injective. If however two different
%% keys point to the same value in Map, then it is not defined, which key will
%% be assigned to the value in ReversedMap.
%%
-spec map_reverse(
    Map :: #{KeyType => ValueType}
) ->
    ReversedMap :: #{ValueType => KeyType}
        when
            KeyType   :: term(),
            ValueType :: term().

map_reverse(Map) ->
    maps:fold(fun(Key, Value, Acc) ->
        Acc#{Value => Key}
    end, #{}, Map).


%%
%%  Returns maximum among the values in map.
%%
-spec map_max_value(
    Map :: #{KeyType => ValueType}
) ->
    MaxValue :: ValueType
        when
            KeyType   :: term(),
            ValueType :: term().

map_max_value(Map) ->
    case maps:next(maps:iterator(Map)) of
        {_, FirstValue, _} ->
            maps:fold(fun(_, Value, Acc) ->
                lists:max([Value, Acc])
            end, FirstValue, Map);
        none ->
            undefined
    end.


%%
%%  Returns minimum among the values in map.
%%
-spec map_min_value(
    Map :: #{KeyType => ValueType}
) ->
    MinValue :: ValueType
        when
            KeyType   :: term(),
            ValueType :: term().

map_min_value(Map) ->
    case maps:next(maps:iterator(Map)) of
        {_, FirstValue, _} ->
            maps:fold(fun(_, Value, Acc) ->
                lists:min([Value, Acc])
            end, FirstValue, Map);
        none ->
            undefined
    end.


%%
%%  Maps every map key-value pair to number and adds those numbers.
%%
-spec map_map_sum(
    MapFun :: fun((Key :: KeyType, Value :: ValueType) -> MapResult :: number()),
    Map    :: #{KeyType => ValueType}
) ->
    Sum :: number()
        when
            KeyType   :: term(),
            ValueType :: term().

map_map_sum(MapFun, Map) ->
    maps:fold(fun(Key, Value, Acc) ->
        Acc + MapFun(Key, Value)
    end, 0, Map).


%%
%%  Filters key-value pairs using FilterFun and counts the remaining ones.
%%
-spec map_map_count(
    FilterFun :: fun((Key :: KeyType, Value :: ValueType) -> boolean()),
    Map       :: #{KeyType => ValueType}
) ->
    Sum :: number()
        when
            KeyType   :: term(),
            ValueType :: term().

map_map_count(FilterFun, Map) ->
    map_map_sum(fun(Key, Value) ->
        case FilterFun(Key, Value) of
            true  -> 1;
            false -> 0
        end
    end, Map).


%%
%%  Returns the index (row and column) of element Elem in matrix Matrix.
%%  If Elem is not found, undefined is returned. If there are several
%%  occurrences of Elem in Matrix, it is not specified, which index is returned.
%%
-spec matrix_index_of(Elem :: ElemType, Matrix :: matrix(ElemType)) ->
        matrix_index() | undefined
    when ElemType :: term().

matrix_index_of(Elem, Matrix) ->
    matrix_index_of_it(Elem, maps:iterator(Matrix)).

matrix_index_of_it(Elem, Iterator) ->
    case maps:next(Iterator) of
        {Index, Elem, _           } -> Index;
        {_,        _, NextIterator} -> matrix_index_of_it(Elem, NextIterator);
        none                        -> undefined
    end.



%%
%%  Checks if index Index is in matrix with dimensions Dimensions.
%%
-spec matrix_is_valid_index(
    CurrentIndex :: matrix_index(),
    Dimensions   :: matrix_index()
) ->
    boolean().

matrix_is_valid_index({ Row, _Col}, {_Rows, _Cols}) when Row<1    -> false;
matrix_is_valid_index({ Row, _Col}, { Rows, _Cols}) when Row>Rows -> false;
matrix_is_valid_index({_Row,  Col}, {_Rows, _Cols}) when Col<1    -> false;
matrix_is_valid_index({_Row,  Col}, {_Rows,  Cols}) when Col>Cols -> false;
matrix_is_valid_index({_Row, _Col}, {_Rows, _Cols})               -> true.


%%
%%  Returns index of the next element of matrix, starting with index Index in
%%  direction Direction. If there is no next element in that direction (it is
%%  out of matrix bounds), undefined is returned.
%%
-spec matrix_next_index(
    CurrentIndex :: matrix_index(),
    Direction    :: matrix_direction(),
    Dimensions   :: matrix_index()
) ->
    NextIndex :: matrix_index() | undefined.

matrix_next_index(CurrentIndex, Direction, Dimensions) ->
    NextIndex = matrix_next_index_no_check(CurrentIndex, Direction),
    case matrix_is_valid_index(NextIndex, Dimensions) of
        true  -> NextIndex;
        false -> undefined
    end.


matrix_next_index_no_check({Row, Col}, up   ) -> {Row-1, Col};
matrix_next_index_no_check({Row, Col}, right) -> {Row, Col+1};
matrix_next_index_no_check({Row, Col}, down ) -> {Row+1, Col};
matrix_next_index_no_check({Row, Col}, left ) -> {Row, Col-1}.


%%
%%  Folds through elements of matrix starting from first row and first column.
%%  Each column of the row is handled consecutively before going to next row.
%%
-spec matrix_foldl(
    FoldFun    :: fun((
                          Index       :: matrix_index(),
                          Elem        :: ElemType,
                          Accumulator :: AccType
                      ) ->
                          NewAccumulator :: AccType
                  ),
    AccIn      :: AccType,
    Matrix     :: matrix(ElemType),
    Dimensions :: matrix_index()
) ->
    AccOut :: AccType
        when
            ElemType :: term(),
            AccType  :: term().

matrix_foldl(FoldFun, AccIn, Matrix, {Rows, Cols}) ->
    lists:foldl(fun(Row, Acc) ->
        lists:foldl(fun(Col, Accc) ->
            Index = {Row, Col},
            FoldFun(Index, maps:get(Index, Matrix), Accc)
        end, Acc, lists:seq(1, Cols))
    end, AccIn, lists:seq(1, Rows)).


%%
%%  Returns all possible directions.
%%
-spec direction_all() -> [matrix_direction()].

direction_all() -> [right, down, left, up].


%%
%%  Returns opposite direction.
%%
-spec direction_reverse(matrix_direction()) -> matrix_direction().

direction_reverse(right) -> left;
direction_reverse(down ) -> up;
direction_reverse(left ) -> right;
direction_reverse(up   ) -> down.


%%
%%  Returns direction after a clockwise turn from initial.
%%
-spec direction_clockwise(matrix_direction()) -> matrix_direction().

direction_clockwise(right) -> down;
direction_clockwise(down ) -> left;
direction_clockwise(left ) -> up;
direction_clockwise(up   ) -> right.


%%
%%  Returns direction after a counterclockwise turn from initial.
%%
-spec direction_counterclockwise(matrix_direction()) -> matrix_direction().

direction_counterclockwise(right) -> up;
direction_counterclockwise(down ) -> right;
direction_counterclockwise(left ) -> down;
direction_counterclockwise(up   ) -> left.


%%
%%  Returns the number of digits in non negative integer.
%%  NOTE: the last case is needed, because `math:log10/1` function returns
%%  float, which has to be converted back to integer and this results in loss
%%  of precision.
%%
-spec integer_digit_count(integer()) -> integer().

integer_digit_count(0) -> 1;
integer_digit_count(Int) when 0 < Int, Int <  ?CUT_INTEGER -> erlang:floor(math:log10(Int)) + 1;
integer_digit_count(Int) when          Int >= ?CUT_INTEGER -> ?CUT_INTEGER_POWER + integer_digit_count(Int div ?CUT_INTEGER).


%%
%%  Returns Power'th power of 10 (10^Power). This function differs from
%%  `math:pow/2` as it returns an integer.
%%  NOTE: the last case is needed, because `math:pow/2` function returns float,
%%  which has to be converted back to integer and this results in loss of
%%  precision.
%%  NOTE: other implementation was considered, but it is much slower:
%%      `erlang:list_to_integer(lists:append(["1"|lists:duplicate(Power, "0")]))`
%%
-spec integer_10_pow(integer()) -> integer().

integer_10_pow(Power) when 0 =< Power, Power <  ?CUT_INTEGER_POWER -> erlang:round(math:pow(10, Power));
integer_10_pow(Power) when             Power >= ?CUT_INTEGER_POWER -> integer_10_pow(Power-?CUT_INTEGER_POWER) * ?CUT_INTEGER.


%%
%%  Returns an integer, which results from concatenating second parameter to
%%  the end of the first parameter, e.g. 123 and 456 results in 123456.
%%
-spec concat_integers(integer(), integer()) -> integer().

concat_integers(Int1, 0) when Int1>0 ->
    Int1*10;

concat_integers(Int1, Int2) when Int1>0, Int2>0 ->
    Power = integer_digit_count(Int2),
    Int1 * integer_10_pow(Power) + Int2.


%%
%%  Splits integer Int into two integers, so that the second integer contains
%%  Split last digits of the initial integer and the first integer contains the
%%  rest digits of Int. Returns `undefined`, if Int does not contain more
%%  digits than Split.
%%
-spec split_integer(Int :: integer(), Split :: integer()) ->
    {integer(), integer()} | undefined.

split_integer(Int, Split) when Int > 9, Split > 0 ->
    Power = integer_10_pow(Split),
    case Int >= Power of
        true  -> {Int div Power, Int rem Power};
        false -> undefined
    end.


%%
%%  Calculates the quotient of euclidean division A/B.
%%
-spec euclidean_div(A :: integer(), B :: integer()) -> integer().

euclidean_div(A, B) when A>0 -> A div B;
euclidean_div(0, _)          -> 0;
euclidean_div(A, B) when A<0 ->
    case A rem B of
        0            -> A div B;
        _ when B > 0 -> A div B - 1;
        _ when B < 0 -> A div B + 1
    end.


%%
%%  Calculates the remainder of euclidean division A/B.
%%
-spec euclidean_rem(A :: integer(), B :: integer()) -> integer().

euclidean_rem(A, B) ->
    case A rem B of
        Pos when Pos >= 0 -> Pos;
        Neg when B > 0    -> Neg + B;
        Neg when B < 0    -> Neg - B
    end.


%%
%%  Solves a single equation A*x=B for x in integers. If there are infinitely
%%  many solutions, returns `any`. If there are no integer solutions, returns
%%  undefined.
%%
-spec solve_one_equation_int({A :: integer(), B :: integer()}) ->
    integer() | any | undefined.

solve_one_equation_int({0, 0}) -> any;
solve_one_equation_int({0, _}) -> undefined;
solve_one_equation_int({A, B}) ->
    case B rem A of
        0 -> B div A;
        _ -> undefined
    end.


%%
%%  Solves two equation system with two variables for x and y in integers:
%%      A1*x + B1*y = C1
%%      A2*x + B2*y = C2
%%  If there are infinitely many possible values for x or y, returns `any` in
%%  that position. If y is dependent on x, returns {any, String}, where String
%%  represents y's dependency on x. If there are no integer solutions, returns
%%  undefined.
%%
-spec solve_two_equations_int(
        {A1 :: integer(), B1 :: integer(), C1 :: integer()},
        {A2 :: integer(), B2 :: integer(), C2 :: integer()}
    ) ->
        {
            X :: integer() | any,
            Y :: integer() | any | string()
        } | undefined.

solve_two_equations_int({0, B1, C1}, {A2, B2, C2}) ->
    % B1*y = C1
    case {solve_one_equation_int({B1, C1}), A2 =:= 0, B2 =:= 0} of
        {undefined, _, _} -> % 0*y=C1 =/= 0 or no integer solutions
            undefined;
        {any, true, _} -> % 0*y=0
            % B2*y=C2
            case solve_one_equation_int({B2, C2}) of
                undefined            -> undefined;  % 0*y=C2 =/= 0 or no integer solutions
                any                  -> {any, any}; % 0*y=0
                Y when is_integer(Y) -> {any, Y}    % B2 =/= 0
            end;
        {any, false, true} ->  % 0*y=0
            % A2*x=C2, A2 =/= 0
            case solve_one_equation_int({A2, C2}) of
                undefined            -> undefined;  % no integer solutions
                X when is_integer(X) -> {X, any}    %
            end;
        {any, false, false} ->  % 0*y=0
            % A2*x+B2*y=C2, A2 =/= 0, B2 =/= 0
            {any, lists:flatten(io_lib:format("(~p - ~p*x)/~p", [C2, A2, B2]))};
        {Y, _, _} -> % B1 =/= 0
            % A2*x=C2-B2*y
            % A2*x=C3
            C3 = C2 - B2*Y,
            case solve_one_equation_int({A2, C3}) of
                undefined            -> undefined;  % 0*y=C3 =/= 0 or no integer solutions
                any                  -> {any, Y};   % 0*y=0
                X when is_integer(X) -> {X, Y}      % A2 =/= 0
            end
    end;

solve_two_equations_int({A1, 0, C1}, {A2, B2, C2}) -> % A1 =/= 0
    case solve_two_equations_int({0, A1, C1}, {B2, A2, C2}) of
        undefined -> undefined;
        {Y, X}    -> {X, Y}
    end;

solve_two_equations_int({A1, B1, C1}, {0, B2, C2}) -> % A1 =/= 0,  % B1 =/= 0
    solve_two_equations_int({0, B2, C2}, {A1, B1, C1});

solve_two_equations_int({A1, B1, C1}, {A2, 0, C2}) -> % A1 =/= 0,  B1 =/= 0, A2 =/= 0
    solve_two_equations_int({A2, 0, C2}, {A1, B1, C1});

solve_two_equations_int({A1, B1, C1}, {A2, B2, C2}) -> % A1 =/= 0,  B1 =/= 0, A2 =/= 0, B2 =/= 0
    % (1) A1*X + B1*Y = C1 /*A2
    % (2) A2*X + B2*Y = C2 /*A1
    % (1)-(2): B3*Y = C3
    B3 = B1*A2-B2*A1,
    C3 = C1*A2-C2*A1,
    case solve_one_equation_int({B3, C3}) of
        undefined ->    % 0*y=C3 =/= 0 or no integer solutions
            undefined;
        any ->  % 0*y=0
            {any, lists:flatten(io_lib:format("(~p - ~p*x)/~p", [C1, A1, B1]))};
        Y ->    % B3 =/= 0
            % A1*X + B1*Y = C1
            % A1*X = C1 - B1*Y
            % A1*X = C4
            C4 = C1 - B1*Y,
            case solve_one_equation_int({A1, C4}) of
                undefined -> undefined;
                X         -> {X, Y}
            end
    end.


%%
%%  Returns binary representation of integer as list of 0s and 1s. The first
%%  element in the returned list is a least significant digit and the last one
%%  is the most significant digit.
%%
-spec integer_to_bits(integer()) -> [bit()].

integer_to_bits(Int) when Int >= 0 -> integer_to_bits(Int, []).
integer_to_bits(0,   Acc)              -> lists:reverse(Acc);
integer_to_bits(Int, Acc) when Int > 0 -> integer_to_bits(Int div 2, [Int rem 2 | Acc]).


%%
%%  Returns integer from its binary representation, provided as list of 0s and
%%  1s. The first element in the parameter list is a least significant digit
%%  and the last one is the most significant digit.
%%
-spec bits_to_integer([bit()]) -> integer().

bits_to_integer(Bits) -> bits_to_integer(lists:reverse(Bits), 0).

bits_to_integer([], Acc)                           -> Acc;
bits_to_integer([Bit|Bits], Acc) when ?IS_BIT(Bit) -> bits_to_integer(Bits, Acc*2+Bit).


%%
%%  Performs and operation for two bits. Compared to band, it checks if
%%  parameters are bits.
%%
-spec bit_and(bit(), bit()) -> bit().

bit_and(D1, D2) -> bit_op(op_and, D1, D2).


%%
%%  Performs or operation for two bits. Compared to bor, it checks if
%%  parameters are bits.
%%
-spec bit_or(bit(), bit()) -> bit().

bit_or(D1, D2) -> bit_op(op_or, D1, D2).


%%
%%  Performs xor operation for two bits. Compared to bxor, it checks if
%%  parameters are bits.
%%
-spec bit_xor(bit(), bit()) -> bit().

bit_xor(D1, D2) -> bit_op(op_xor, D1, D2).


%%
%%  Performs bitwise and operation for two binary numbers, provided as list of
%%  0s and 1s.
%%
-spec bits_and([bit()], [bit()]) -> [bit()].

bits_and(Bits1, Bits2) -> bits_op(op_and, Bits1, Bits2).


%%
%%  Performs bitwise or operation for two binary numbers, provided as list of
%%  0s and 1s.
%%
-spec bits_or([bit()], [bit()]) -> [bit()].

bits_or(Bits1, Bits2) -> bits_op(op_or, Bits1, Bits2).


%%
%%  Performs bitwise xor operation for two binary numbers, provided as list of
%%  0s and 1s.
%%
-spec bits_xor([bit()], [bit()]) -> [bit()].

bits_xor(Bits1, Bits2) -> bits_op(op_xor, Bits1, Bits2).


bit_op(op_and, D1, D2) when ?IS_BIT(D1), ?IS_BIT(D2) -> D1 band D2;
bit_op(op_or,  D1, D2) when ?IS_BIT(D1), ?IS_BIT(D2) -> D1 bor  D2;
bit_op(op_xor, D1, D2) when ?IS_BIT(D1), ?IS_BIT(D2) -> D1 bxor D2.


bits_op(Op, Bits1, Bits2) -> bits_op(Op, Bits1, Bits2, []).

bits_op(_Op, [],           [],           Acc)                                   -> lists:reverse(drop_start_zeros(Acc));
bits_op( Op, [Bit1|Bits1], [Bit2|Bits2], Acc) when ?IS_BIT(Bit1), ?IS_BIT(Bit2) -> bits_op(Op, Bits1, Bits2, [bit_op(Op, Bit1, Bit2) | Acc]);
bits_op( Op, [],           [Bit2|Bits2], Acc) when ?IS_BIT(Bit2)                -> bits_op(Op, [],    Bits2, [bit_op(Op, 0,    Bit2) | Acc]);
bits_op( Op, [Bit1|Bits1], [],           Acc) when ?IS_BIT(Bit1)                -> bits_op(Op, Bits1, [],    [bit_op(Op, Bit1, 0   ) | Acc]).


drop_start_zeros([0])        -> [0];
drop_start_zeros([0|Digits]) -> drop_start_zeros(Digits);
drop_start_zeros([1|Digits]) -> [1|Digits].


%%
%%  Inverts a single bit.
%%
-spec bit_invert(bit()) -> bit().

bit_invert(1) -> 0;
bit_invert(0) -> 1.
