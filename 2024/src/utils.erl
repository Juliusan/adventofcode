-module(utils).
-export([print/2, print_char_matrix/2]).
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
-export([get_integer/1, get_integer_list/1, get_integer_list/2, get_char_matrix/1]).
-export([drop_trailing_new_line/1]).
-export([count_elems_sorted/2, count_elems_start/1]).
-export([is_decreasing/2, is_increasing/2]).
-export([transpose/1, diagonals_f/1, diagonals_b/1, middle/1, middle_single/1, foldl_pairs/3]).
-export([matrix_index_of/2, matrix_is_valid_index/2, matrix_next_index/3, matrix_foldl/4]).
-export([integer_digit_count/1, integer_10_pow/1, concat_integers/2, split_integer/2]).


-type matrix(Type) :: #{matrix_index() => Type}.
-type matrix_index() :: {Row :: integer(), Column :: integer()}.
-type matrix_direction() :: up | right | down | left.


-define(CUT_INTEGER,       1_000_000_000_000).
-define(CUT_INTEGER_POWER, 12).


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
-spec middle_single(List :: [ElemType]) -> ElemType.

middle_single(List) ->
    [Middle] = middle(List),
    Middle.


%%
%%  Folds through every pair of different elements of list List. If List is
%%  [1,2,3,...,n], then the fold order ir {1,2}, {1,3}, ..., {1,n}, {2,3}, ...,
%%  {2,n},...
%%
-spec foldl_pairs(
    FoldFun :: fun((
                       Elem1       :: ElemType,
                       Elem2       :: ElemType,
                       Accumulator :: AccType
                   ) ->
                       NewAccumulator :: AccType
               ),
    AccIn   :: AccType,
    List    :: list(ElemType)
) ->
    AccOut :: AccType
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
