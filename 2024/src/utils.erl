-module(utils).
-export([print/2]).
-export([read_single_line/1, read_lines/1, read_line_to_elem/2]).
-export([get_integer/1, get_integer_list/1]).
-export([count_elems_sorted/2, count_elems_start/1]).
-export([is_decreasing/2, is_increasing/2]).


%%
%%  Outputs message to terminal.
%% 
print(Pattern, Params) -> io:fwrite(Pattern ++ "~n", Params).


%%
%%  Reads single line from file FileName.
%% 
-spec read_single_line(
    FileName :: string()
) ->
    Line :: string().

read_single_line(FileName) ->
    {ok, File} = file:open(FileName, [read]),
    {ok, Line} = file:read_line(File),
    ok = file:close(File),
    Line.


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
