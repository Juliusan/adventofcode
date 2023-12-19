-module(day19_2).
-export([solve/1]).

% Irgi nesudėtinga. Tik programavimas.

solve(FileName) ->
    Flows = get_input(FileName),
    Result = get_accepted(Flows),
    Result.
    
    
get_input(FileName) ->
    {ok, File} = file:open(FileName, [read]),
    Flows = get_input_flows(File, #{}),
    ok = file:close(File),
    Flows.

    
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


get_accepted(Flows) ->
    get_accepted(Flows, {{1,4000},{1,4000},{1,4000},{1,4000}}, "in").
    
    
get_accepted(_Flows, {{XS, XE},{MS, ME},{AS, AE},{SS, SE}}, accept) -> (XE-XS+1)*(ME-MS+1)*(AE-AS+1)*(SE-SS+1);
get_accepted(_Flows, _Ranges,                               reject) -> 0;
get_accepted( Flows,  Ranges,                               Name) ->
    #{Name := Flow} = Flows,
    apply_flow(Flow, Ranges, Flows).
    
    
apply_flow([{true, Last}], Ranges, Flows) -> get_accepted(Flows, Ranges, Last);
apply_flow([{{Index, Sign, Number}, Result}|Rules], Ranges, Flows) ->
    {From,To} = erlang:element(Index, Ranges),
    {RangeHold, RangeNot} = case Sign of
        less when Number =< From -> {empty,            {From,   To}};
        less when Number =< To   -> {{From, Number-1}, {Number, To}};
        less                     -> {{From, To},       empty};
        more when Number < From  -> {{From, To},       empty};
        more when Number < To    -> {{Number+1, To},   {From, Number}};
        more                     -> {empty,            {From, To}}
    end,
    HoldSum = case RangeHold of
        empty -> 0;
        _ ->
            RangesHold = erlang:setelement(Index, Ranges, RangeHold),
            get_accepted(Flows, RangesHold, Result)
    end,
    NotSum = case RangeNot of
        empty -> 0;
        _ ->
            RangesNot = erlang:setelement(Index, Ranges, RangeNot),
            apply_flow(Rules, RangesNot, Flows)
    end,
    HoldSum+NotSum.


trim_ending_newline(Str) ->
    string:sub_string(Str, 1, string:len(Str) - 1).

