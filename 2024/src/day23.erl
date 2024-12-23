-module(day23).
-export([solve_1/1, solve_2/1]).

% Perskaitęs sąlygą išsigandau. Pirmai daliai sugalvojau algoritmą, bet truputį
% ne taip bandžiau optimizuouti. Bandžiau mesti ryšius iš tinklo. Ir gaudavau
% arba per daug (besidubliuojančių trejetų, arba per mažai, nes kažką per daug
% būdavau išmetęs. Vėliau sugalvojau vieto metimo tiesiog rūšiuoti kompiuterių
% vardus. Ir atsakymas gavosi iš karto. Na o antrą dalį parašiau labai greitai.
% Gaila tik kad sąlygos neįsiskaičiau ir 3/4 laiko ieškojau klaidos, kurios iš
% tiesų nebuvo. Aš tik atsakymą ne taip pateikdavau.

% (aoc_2024@JuliusErisataT14.erisata.lt)1> day23:solve_1("priv/day23-PVZ.txt").
% 7
% (aoc_2024@JuliusErisataT14.erisata.lt)2> day23:solve_1("priv/day23.txt").
% 1238
% (aoc_2024@JuliusErisataT14.erisata.lt)3> day23:solve_2("priv/day23-PVZ.txt").
% "co,de,ka,ta"
% (aoc_2024@JuliusErisataT14.erisata.lt)4> day23:solve_2("priv/day23.txt").
% "bg,bl,ch,fn,fv,gd,jn,kk,lk,pv,rr,tb,vw"
% (aoc_2024@JuliusErisataT14.erisata.lt)5> timer:tc(fun() -> day23:solve_1("priv/day23.txt") end).
% {41331,1238}
% (aoc_2024@JuliusErisataT14.erisata.lt)6> timer:tc(fun() -> day23:solve_2("priv/day23.txt") end).
% {236087,"bg,bl,ch,fn,fv,gd,jn,kk,lk,pv,rr,tb,vw"}


read_inputs(FileName) ->
    utils:read_lines_no_new_line_to_elems(FileName, fun(Line) ->
        [Comp1, Comp2] = string:split(Line, "-", all),
        {Comp1, Comp2}
    end).


connections_to_map(Connections) ->
    lists:foldl(fun({Comp1, Comp2}, Acc) ->
        Comp1Cons = maps:get(Comp1, Acc, []),
        Comp2Cons = maps:get(Comp2, Acc, []),
        NewComp1Cons = [Comp2 | Comp1Cons],
        NewComp2Cons = [Comp1 | Comp2Cons],
        Acc#{Comp1 => NewComp1Cons, Comp2 => NewComp2Cons}
    end, #{}, Connections).


find_three_way_network(ConnMap) ->
    Networks = maps:fold(fun(Comp1, Comp1Cons, AccNetworks1) ->
        lists:foldl(fun(Comp2, AccNetworks2) ->
            case Comp1 < Comp2 of
                true ->
                    Comp2Cons = maps:get(Comp2, ConnMap, []),
                    Intersect = [ Comp || Comp <- Comp1Cons, true =:= lists:member(Comp, Comp2Cons) ],
                    lists:foldl(fun(Comp3, AccNetworks3) ->
                        case Comp2 < Comp3 of
                            true ->
                                case maps:get(Comp3, ConnMap, undefined) of
                                    undefined  -> AccNetworks3;
                                    _Comp3Cons -> [{Comp1, Comp2, Comp3} | AccNetworks3]
                                end;
                            false ->
                                AccNetworks3
                        end
                    end, AccNetworks2, Intersect);
                false ->
                    AccNetworks2
            end
        end, AccNetworks1, Comp1Cons)
    end, [], ConnMap),
    Networks.


find_largest_network(ConnMap) ->
    Keys = maps:keys(ConnMap),
    find_largest_network(Keys, Keys, [], ConnMap).

find_largest_network([], A, AccNetwork, _ConnMap) ->
    %utils:print("---~n~p~n~p~n...", [A, AccNetwork]),
    %
    AccNetwork;

find_largest_network(Comps, CompInter, AccNetwork, ConnMap) ->
    lists:foldl(fun(Comp, Acc) ->
        CompConns = maps:get(Comp, ConnMap),
        Intersect = [ C || C <- CompInter, true =:= lists:member(C, CompConns) ],
        IntersectGood = lists:filter(fun(C) -> Comp < C end, Intersect),
        Largest = find_largest_network(IntersectGood, Intersect, [Comp|AccNetwork], ConnMap),
        case erlang:length(Largest) > erlang:length(Acc) of
            true  -> Largest;
            false -> Acc
        end
    end, [], Comps).



is_ts(Network) ->
    case Network of
        {[$t|_], _, _} -> true;
        {_, [$t|_], _} -> true;
        {_, _, [$t|_]} -> true;
        {_, _,      _} -> false
    end.


solve_1(FileName) ->
    Connections = read_inputs(FileName),
    ConnMap = connections_to_map(Connections),
    Networks = find_three_way_network(ConnMap),
    %lists:foreach(fun(Network) -> utils:print("~p", [Network]) end, Networks),
    utils:list_filter_count(fun is_ts/1, Networks).


solve_2(FileName) ->
    Connections = read_inputs(FileName),
    ConnMap = connections_to_map(Connections),
    %NewConnMap = maps:map(fun(Key, Value) -> [Key|Value] end, ConnMap),
    Network = find_largest_network(ConnMap),
    NetworkS = lists:sort(Network),
    lists:append(lists:join(",", NetworkS)).