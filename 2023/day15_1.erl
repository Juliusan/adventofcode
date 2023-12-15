-module(day15_1).
-export([solve/1]).

% Labai paprastas. Tik programavimas.

solve(FileName) ->
    Strings = get_strings(FileName),
    HashSum = count_strings_hash(Strings),
    HashSum.
    
    
get_strings(FileName) ->
    {ok, File} = file:open(FileName, [read]),
    {ok, Line} = file:read_line(File),
    eof = file:read_line(File),
    LineNoNewLine = trim_ending_newline(Line),
    Strings = string:split(LineNoNewLine, ",", all),
    Strings.
    
    
count_strings_hash(Map) ->
    count_strings_hash(Map, 0).
    
count_strings_hash([], AccSum) ->
    AccSum;
    
count_strings_hash([String|Strings], AccSum) ->
    Length = erlang:length(String),
    StringHash = count_string_hash(String, Length, 0),
    NewAccSum = StringHash + AccSum,
    count_strings_hash(Strings, NewAccSum).
    
    
count_string_hash("", _Length, AccSum) ->
    AccSum rem 256;
    
count_string_hash([Char|String], Length, AccSum) ->
    %io:fwrite("XXX ~p ~p ~p~n", [Char, Length, AccSum]),
    NewAccSum = (AccSum+Char)*17,
    count_string_hash(String, Length-1, NewAccSum).


trim_ending_newline(Str) ->
    string:sub_string(Str, 1, string:len(Str) - 1).

