-module(utils).
-export([print/2, print_char_matrix/3]).
-export([
    read_file/1,
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
-export([transpose/1, diagonals_f/1, diagonals_b/1, middle/1, middle_single/1]).
-export([matrix_index_of/2, matrix_next_index/5, matrix_foldl/5]).
-export([concat_integers/2]).


-type matrix(Type) :: #{{integer(), integer()} => Type}.
-type matrix_direction() :: up | right | down | left.


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
%%  Outputs matrix Matrix, which has NumberOfRows rows and NumberOfColumns columns to terminal.
%%
-spec print_char_matrix(
    Matrix          :: matrix(term()),
    NumberOfRows    :: integer(),
    NumberOfColumns :: integer()
) ->
    ok.

print_char_matrix(Matrix, Rows, Cols) ->
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

get_integer(Line) -> get_integer(Line, 0).

get_integer([Digit|Else], Acc) when $0 =< Digit, Digit =< $9 -> get_integer(Else, Acc*10 + Digit - $0);
get_integer(Line,         Acc)                               -> {Acc, Line}.


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
%%  Line number in list is the first element of index ("row"), character's position
%%  in line ("column") is the second element. Total number of rows and columns is
%%  also returned. It is expected (although not necessary for this function) that
%%  each matrix row contains the same number of elements.
%%
-spec get_char_matrix([Line :: string()]) ->
    {
        Matrix          :: matrix(char()),
        NumberOfRows    :: integer(),
        NumberOfColumns :: integer()
    }.

get_char_matrix([]) ->
    {#{}, 0, 0};

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
    {Matrix, Rows1-1, Cols}.


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
%%  Returns the position (row and column) of element Elem in matrix Matrix.
%%  If Elem is not found, undefined is returned. If there are several
%%  occurrences of Elem in Matrix, it is not specified, which index is returned.
%%
-spec matrix_index_of(Elem :: ElemType, Matrix :: matrix(ElemType)) ->
        {Row :: integer(), Column :: integer()} | undefined
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
%%  Returns index of the next element of matrix, starting with row Row and
%%  column Column in direction Direction. If there is no next element in
%%  that direction (it is out of matrix bounds), undefined is returned.
%%
-spec matrix_next_index(
    Row             :: integer(),
    Column          :: integer(),
    Direction       :: matrix_direction(),
    NumberOfRows    :: integer(),
    NumberOfColumns :: integer()
) ->
    {NextRow :: integer(), NextColumn :: integer()} | undefined.

matrix_next_index( 1,    _Col,  up   , _Rows, _Cols)               -> undefined;
matrix_next_index( Row,   Col,  up   , _Rows, _Cols) when Row>1    -> {Row-1, Col};
matrix_next_index(_Row,   Cols, right, _Rows,  Cols)               -> undefined;
matrix_next_index( Row,   Col,  right, _Rows,  Cols) when Col<Cols -> {Row, Col+1};
matrix_next_index( Rows, _Col,  down ,  Rows, _Cols)               -> undefined;
matrix_next_index( Row,   Col,  down ,  Rows, _Cols) when Row<Rows -> {Row+1, Col};
matrix_next_index(_Row,   1,    left , _Rows, _Cols)               -> undefined;
matrix_next_index( Row,   Col,  left , _Rows, _Cols) when Col>1    -> {Row, Col-1}.


%%
%%  Folds through elements of matrix starting from first row and first column.
%%  Each column of the row is handled consecutively before going to next row.
%%
-spec matrix_foldl(
    FoldFun          :: fun((
                                Row         :: integer(),
                                Column      :: integer(),
                                Elem        :: ElemType,
                                Accumulator :: AccType
                            ) ->
                                NewAccumulator :: AccType
                        ),
    AccIn            :: AccType,
    Matrix           :: matrix(ElemType),
    NumberOfRows     :: integer(),
    NumberOfColumns  :: integer()
) ->
    AccOut :: AccType
        when
            ElemType :: term(),
            AccType  :: term().

matrix_foldl(FoldFun, AccIn, Matrix, Rows, Cols) ->
    lists:foldl(fun(Row, Acc) ->
        lists:foldl(fun(Col, Accc) ->
            FoldFun(Row, Col, maps:get({Row, Col}, Matrix), Accc)
        end, Acc, lists:seq(1, Cols))
    end, AccIn, lists:seq(1, Rows)).


%%
%%  Returns an integer, which results from concating second parameter to the end
%%  of the first parameter, e.g. 123 and 456 results in 123456.
%%
-spec concat_integers(integer(), integer()) -> integer().

concat_integers(Int1, 0) when Int1>0 ->
    Int1*10;

concat_integers(Int1, Int2) when Int1>0, Int2>0 ->
    Power = erlang:round(math:floor(math:log10(Int2))) + 1,
    Power10 = erlang:list_to_integer(lists:append(["1"|lists:duplicate(Power, "0")])),
    Int1 * Power10 + Int2.
