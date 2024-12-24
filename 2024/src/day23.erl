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
    Connections = utils:read_lines_no_new_line_to_elems(FileName, fun(Line) ->
        [Comp1, Comp2] = string:split(Line, "-", all),
        {Comp1, Comp2}
    end),
    connections_to_map(Connections).


connections_to_map(Connections) ->
    lists:foldl(fun({Comp1, Comp2}, Acc) ->
        Comp1Conns = maps:get(Comp1, Acc, []),
        Comp2Conns = maps:get(Comp2, Acc, []),
        Acc#{
            Comp1 => [Comp2 | Comp1Conns],
            Comp2 => [Comp1 | Comp2Conns]
        }
    end, #{}, Connections).


find_three_way_network(ConnMap) ->
    Networks = maps:fold(fun(Comp1, Comp1Conns, AccNetworks1) ->
        lists:foldl(fun(Comp2, AccNetworks2) ->
            case Comp1 < Comp2 of
                true ->
                    Comp2Conns = maps:get(Comp2, ConnMap, []),
                    Intersection = utils:intersection(Comp1Conns, Comp2Conns),
                    lists:foldl(fun(Comp3, AccNetworks3) ->
                        case Comp2 < Comp3 of
                            true  -> [{Comp1, Comp2, Comp3} | AccNetworks3];
                            false -> AccNetworks3
                        end
                    end, AccNetworks2, Intersection);
                false ->
                    AccNetworks2
            end
        end, AccNetworks1, Comp1Conns)
    end, [], ConnMap),
    Networks.


find_largest_network(ConnMap) ->
    find_largest_network(maps:keys(ConnMap), [], ConnMap).

find_largest_network([], AccNetwork, _ConnMap) ->
    AccNetwork;

find_largest_network(Comps, AccNetwork, ConnMap) ->
    lists:foldl(fun(Comp, Acc) ->
        CompConns = maps:get(Comp, ConnMap),
        Intersection = utils:intersection(Comps, CompConns),
        ValidComps = lists:filter(fun(C) -> Comp < C end, Intersection),
        Largest = find_largest_network(ValidComps, [Comp|AccNetwork], ConnMap),
        case erlang:length(Largest) > erlang:length(Acc) of
            true  -> Largest;
            false -> Acc
        end
    end, [], Comps).



is_ts({[$t|_], _, _}) -> true;
is_ts({_, [$t|_], _}) -> true;
is_ts({_, _, [$t|_]}) -> true;
is_ts({_, _,      _}) -> false.


solve_1(FileName) ->
    ConnMap = read_inputs(FileName),
    Networks = find_three_way_network(ConnMap),
    utils:list_filter_count(fun is_ts/1, Networks).


solve_2(FileName) ->
    ConnMap = read_inputs(FileName),
    Network = find_largest_network(ConnMap),
    NetworkS = lists:sort(Network),
    string:join(NetworkS, ",").