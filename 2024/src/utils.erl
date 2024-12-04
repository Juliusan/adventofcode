-module(utils).
-export([print/2]).
-export([read_file/1, read_lines/1, read_line_to_elem/2]).
-export([get_integer/1, get_integer_list/1]).
-export([count_elems_sorted/2, count_elems_start/1]).
-export([is_decreasing/2, is_increasing/2]).
-export([transpose/1, diagonals_f/1, diagonals_b/1]).


%%
%%  Outputs message to terminal.
%% 
print(Pattern, Params) -> io:fwrite(Pattern ++ "~n", Params).


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

read_lines(FileName) -> read_line_to_elem(FileName, fun(Line) -> Line end).


%%
%%  Reads file FileName and each line converts to a single element in the resulting list, using LineToElemFun.
%%  The returned list is in reversed order compared to input file.
%% 
-spec read_line_to_elem(
    FileName      :: string(),
    LineToElemFun :: fun((Line :: string()) -> ElemType)
) ->
    [ElemType] when
        ElemType :: term().
        
read_line_to_elem(FileName, LineToElemFun) ->
    {ok, File} = file:open(FileName, [read]),
    ResultFun = fun ResultFun(List) ->
        case file:read_line(File) of
            eof ->
                List;
            {ok, Line} ->
                Elem = LineToElemFun(Line),
                ResultFun([Elem | List])
        end
    end,
    Result = ResultFun([]),
    ok = file:close(File),
    Result.


%%
%%  Reads an integer from the string until non digit character is encountered.
%%  Fails, if any other symbol is present. Returns remaining symbols in the line.
%% 
-spec get_integer(string()) -> {integer(), Remaining :: string()}.

get_integer(Line) -> get_integer(Line, 0).

get_integer([Digit|Else], Acc) when $0 =< Digit, Digit =< $9 -> get_integer(Else, Acc*10 + Digit - $0);
get_integer(Line,         Acc)                               -> {Acc, Line}.


%%
%%  Reads a list of space separated integers. Returns the list in reverse order.
%% 
-spec get_integer_list(string()) -> [integer()].

get_integer_list(Line) -> get_integer_list(Line, []).

get_integer_list("", Acc) ->
    Acc;

get_integer_list(Line, Acc) ->
    {Nr, NewLine1} = get_integer(Line),
    NewLine2 = string:trim(NewLine1, leading),
    get_integer_list(NewLine2, [Nr | Acc]).


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