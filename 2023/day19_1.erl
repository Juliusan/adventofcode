-module(day19_1).
-export([solve/1]).

% Nieko sudėtingo. Tik programavimas.

solve(FileName) ->
    {Flows, Parts} = get_input(FileName),
    Result = calculate_parts(Flows, Parts),
    Result.
    
    
get_input(FileName) ->
    {ok, File} = file:open(FileName, [read]),
    Flows = get_input_flows(File, #{}),
    Parts = get_input_parts(File, []),
    ok = file:close(File),
    {Flows, Parts}.

    
get_input_flows(File, AccFlows) ->
    case file:read_line(File) of
        {ok, "\n"} ->
            AccFlows;
        {ok, Line} ->
            %io:fwrite("XXX ~p: ", [Line]),
            FlowLine = trim_ending_newline(Line),
            [Name,RulesStrPlus] = string:split(FlowLine, "{"),
            [RulesStr, ""] = string:split(RulesStrPlus, "}"),
            Rules = get_input_flows_rules(RulesStr),
            get_input_flows(File, AccFlows#{Name => Rules})
    end.
    
    
get_input_flows_rules(RulesStr) ->
    get_input_flows_rules(string:split(RulesStr, ",", all), []).
    
get_input_flows_rules([Last], AccRules) -> lists:reverse([{true, get_rule_result(Last)}|AccRules]);
get_input_flows_rules([RuleStr|Rules], AccRules) ->
    [Condition, ResultStr] = string:split(RuleStr, ":"),
    [PartStr, SignStr | NumberStr] = Condition,
    Part = case PartStr of
        $x -> 1;
        $m -> 2;
        $a -> 3;
        $s -> 4
    end,
    Sign = case SignStr of
        $< -> less;
        $> -> more
    end,
    Number = erlang:list_to_integer(NumberStr),
    Result = get_rule_result(ResultStr),
    Rule = {{Part, Sign, Number}, Result},
    get_input_flows_rules(Rules, [Rule|AccRules]).
        

get_rule_result("A")  -> accept;
get_rule_result("R")  -> reject;
get_rule_result(Name) -> Name.

    
get_input_parts(File, AccParts) ->
    case file:read_line(File) of
        eof ->
            AccParts;
        {ok, Line} ->
            %io:fwrite("XXX ~p: ", [Line]),
            PartLine = trim_ending_newline(Line),
            ["",PartLineLTrimed] = string:split(PartLine, "{"),
            [PartLineTrimed, ""] = string:split(PartLineLTrimed, "}"),
            ["x="++XNumberStr, "m="++MNumberStr, "a="++ANumberStr, "s="++SNumberStr] = string:split(PartLineTrimed, ",", all),
            [XNumber, MNumber, ANumber, SNumber] = lists:map(fun erlang:list_to_integer/1, [XNumberStr, MNumberStr, ANumberStr, SNumberStr]),
            Part = {XNumber, MNumber, ANumber, SNumber},
            get_input_parts(File, [Part|AccParts])
    end.


calculate_parts(Flows, Parts) ->
    calculate_parts(Flows, Parts, 0).
    
calculate_parts(_Flows, [], AccSum) ->
    AccSum;
    
calculate_parts(Flows, [{X, M, A, S} = Part|Parts], AccSum) ->
    NewAccSum = case calculate_part(Flows, Part, "in") of
        accept -> AccSum + X+M+A+S;
        reject -> AccSum
    end,
    calculate_parts(Flows, Parts, NewAccSum).
    
    
calculate_part(_Flows, _Part, accept) -> accept;
calculate_part(_Flows, _Part, reject) -> reject;
calculate_part( Flows,  Part, Name) ->
    %io:fwrite("XXX ~p ~p~n", [Part, Name]),
    #{Name := Flow} = Flows,
    Next = apply_flow(Flow, Part),
    calculate_part(Flows, Part, Next).
    
    
apply_flow([{true, Last}], _Part) -> Last;
apply_flow([{{Index, Sign, Number}, Result}|Rules], Part) ->
    PartNumber = erlang:element(Index, Part),
    Holds = case Sign of
        less -> PartNumber < Number;
        more -> PartNumber > Number
    end,
    case Holds of
        true  -> Result;
        false -> apply_flow(Rules, Part)
    end.


trim_ending_newline(Str) ->
    string:sub_string(Str, 1, string:len(Str) - 1).

