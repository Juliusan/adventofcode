-module(day15_2).
-export([solve/1]).

% Ne ką sudėtingesnis už pirmą dalį. Man labai įprasta tema. Truko tik programavimas.

solve(FileName) ->
    Strings = get_strings(FileName),
    %io:fwrite("XXX ~p~n", [Strings]),
    Boxes = make_boxes(Strings),
    %io:fwrite("XXX ~p~n", [Boxes]),
    FPower = count_power(Boxes),
    FPower.
    
    
get_strings(FileName) ->
    {ok, File} = file:open(FileName, [read]),
    {ok, Line} = file:read_line(File),
    eof = file:read_line(File),
    LineNoNewLine = trim_ending_newline(Line),
    Strings = string:split(LineNoNewLine, ",", all),
    Strings.
    
    
make_boxes(Strings) ->
    InitBoxes = lists:foldl(fun(Hash, AccBoxes) ->
        AccBoxes#{Hash => []}
    end, #{}, lists:seq(0,255)),
    lists:foldl(fun(String, AccBoxes) ->
        case parse_operation(String) of
            {add, Name, FLength} ->
                NameHash = count_string_hash(Name),
                #{NameHash := Lenses} = AccBoxes,
                NewLenses = case lists:keyfind(Name, 1, Lenses) of
                    false               -> [{Name, FLength}|Lenses];
                    {Name, _OldFLength} -> lists:keyreplace(Name, 1, Lenses, {Name, FLength})
                end,
                AccBoxes#{NameHash := NewLenses};
            {remove, Name} ->
                NameHash = count_string_hash(Name),
                #{NameHash := Lenses} = AccBoxes,
                NewLenses = lists:keydelete(Name, 1, Lenses),
                AccBoxes#{NameHash := NewLenses}
        end
    end, InitBoxes, Strings).
    
    
count_power(Boxes) ->
    maps:fold(fun(Hash, Lenses, AccPower) ->
        Hash1 = Hash+1,
        InitIndex = erlang:length(Lenses),
        {_, Power} = lists:foldl(fun({_Name, FLength}, {Index, AccP}) ->
            P = Hash1*FLength*Index,
            {Index-1, P + AccP}
        end, {InitIndex, 0}, Lenses),
        Power + AccPower
    end, 0, Boxes).
    
    
parse_operation([$-]) ->
    {remove, ""};
    
parse_operation([$=|FLengthStr]) ->
    FLength = erlang:list_to_integer(FLengthStr),
    {add, "", FLength};
    
parse_operation([NameChar|String]) ->
    case parse_operation(String) of
        {remove, Name}       -> {remove, [NameChar|Name]};
        {add, Name, FLength} -> {add, [NameChar|Name], FLength}
    end.


count_string_hash(String) ->
    Length = erlang:length(String),
    count_string_hash(String, Length, 0).
    
count_string_hash("", _Length, AccSum) ->
    AccSum rem 256;
    
count_string_hash([Char|String], Length, AccSum) ->
    %io:fwrite("XXX ~p ~p ~p~n", [Char, Length, AccSum]),
    NewAccSum = (AccSum+Char)*17,
    count_string_hash(String, Length-1, NewAccSum).


trim_ending_newline(Str) ->
    string:sub_string(Str, 1, string:len(Str) - 1).

