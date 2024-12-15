-module(day14).
-export([solve_1/2, solve_2/2]).

% Pirma dalis ir vėl buvo lengva, matematinė. Šį kartą ir vėl prižioplinau ir
% iš pirmo karto neteisingai vidurį suskaičiavau. Kiek užtruko, kol tai
% supratau. O antra dalis... mano skoniui trūko aprašymo, o kas gi yra eglutė.
% Pažiūrėjau filmuką apie taip, kokią eglutę rado kiti ir pabandžiau tokios
% paieškoti. Radau tris viršūnės eilutes ir tada rezultatą vizualiai patikrinau.

% (aoc_2024@JuliusErisataT14.erisata.lt)1> day14:solve_1("priv/day14-PVZ.txt", {11,7}).
% 12
% (aoc_2024@JuliusErisataT14.erisata.lt)2> day14:solve_1("priv/day14.txt", {101,103}).
% 225943500
% (aoc_2024@JuliusErisataT14.erisata.lt)3> day14:solve_2("priv/day14.txt", {101,103}).
% ...
% ³                                                                          *        *                 ³
% ³                                                *                                                    ³
% ³                                                                                                     ³
% ³                           *                 *******************************                         ³
% ³                                             *                             *                         ³
% ³                                      *      *                             *                  *      ³
% ³                                             *                             *                         ³
% ³                 *  *             *          *                             *                         ³
% ³                                             *              *              *              *          ³
% ³*                                        *   *             ***             *                         ³
% ³                 *                           *            *****            *                         ³
% ³                                             *           *******           *                         ³
% ³                       *                     *          *********          *                         ³
% ³                                             *            *****            *                *        ³
% ³                                             *           *******           *                         ³
% ³                                           * *          *********          *                  *      ³
% ³                                             *         ***********         *                     *   ³
% ³   *                                  *      *        *************        *                         ³
% ³          *                                  *          *********          *                         ³
% ³                        *                    *         ***********         *                         ³
% ³             *              *                *        *************        *               *         ³
% ³                                             *       ***************       *     *                   ³
% ³                                             *      *****************      *                    *    ³
% ³                                             *        *************        *                         ³
% ³               *                             *       ***************       *                         ³
% ³                         *                   *      *****************      *                      *  ³
% ³                                *            *     *******************     *           *             ³
% ³   *                            *            *    *********************    *                         ³
% ³                                *            *             ***             *        *                ³
% ³                                 *           *             ***             *                         ³
% ³                                             *             ***             *                         ³
% ³                                             *                             *                         ³
% ³                                             *                             *                         ³
% ³     **                                      *                             *                         ³
% ³      *                                      *                             *                         ³
% ³                                             *******************************            *            ³
% ³              *                                                                                      ³
% ³                                  * *    *                                                           ³
% ³                     *   *  *                                                                        ³
% ...
% 6377
% (aoc_2024@JuliusErisataT14.erisata.lt)4> timer:tc(fun() -> day14:solve_1("priv/day14.txt", {101,103}) end).
% {3320,225943500}
% (aoc_2024@JuliusErisataT14.erisata.lt)5> timer:tc(fun() -> day14:solve_2("priv/day14.txt", {101,103}) end).
% ...
% {1035713,6377}


read_inputs(FileName) ->
    utils:read_lines_to_elems(FileName, fun(Line) ->
        "p=" ++ Line1 = Line,
        {PosX, "," ++ Line2} = utils:get_integer(Line1),
        {PosY, " v=" ++ Line3} = utils:get_integer(Line2),
        {VelX, "," ++ Line4} = utils:get_integer(Line3),
        {VelY, "\n"} = utils:get_integer(Line4),
        {PosX, PosY, VelX, VelY}
    end).


move_robots(Robots, Seconds, {Width, Height}) ->
    lists:map(fun({PosX, PosY, VelX, VelY}) ->
        NewPosX = utils:euclidean_rem(PosX+VelX*Seconds, Width),
        NewPosY = utils:euclidean_rem(PosY+VelY*Seconds, Height),
        {NewPosX, NewPosY, VelX, VelY}
    end, Robots).


count_quadrants(Robots, {Width, Height}) ->
    MiddleW = Width div 2,
    MiddleH = Height div 2,
    {Q1, Q2, Q3, Q4} = lists:foldl(fun
        ({PosX, PosY, _, _}, {AccQ1, AccQ2, AccQ3, AccQ4}) when PosX < MiddleW, PosY < MiddleH -> {AccQ1+1, AccQ2,   AccQ3,   AccQ4  };
        ({PosX, PosY, _, _}, {AccQ1, AccQ2, AccQ3, AccQ4}) when PosX > MiddleW, PosY < MiddleH -> {AccQ1,   AccQ2+1, AccQ3,   AccQ4  };
        ({PosX, PosY, _, _}, {AccQ1, AccQ2, AccQ3, AccQ4}) when PosX < MiddleW, PosY > MiddleH -> {AccQ1,   AccQ2,   AccQ3+1, AccQ4  };
        ({PosX, PosY, _, _}, {AccQ1, AccQ2, AccQ3, AccQ4}) when PosX > MiddleW, PosY > MiddleH -> {AccQ1,   AccQ2,   AccQ3,   AccQ4+1};
        ({_,    _,    _, _}, {AccQ1, AccQ2, AccQ3, AccQ4})                                     -> {AccQ1,   AccQ2,   AccQ3,   AccQ4  }
    end, {0, 0, 0, 0}, Robots),
    Q1*Q2*Q3*Q4.


print_robots(Robots, {Cols, Rows}) ->
    EmptyMap1 = utils:get_new_matrix($ , {Rows+1, Cols+1}),
    % corners
    EmptyMap2 = EmptyMap1#{{1,1}=>$+, {1,Cols+2}=>$+, {Rows+2, 1}=>$+, {Rows+2,Cols+2}=>$+},
    % top and botom
    EmptyMap3 = lists:foldl(fun(Col, Acc) ->
        Acc#{{1,Col}=>$-, {Rows+2,Col}=>$-}
    end, EmptyMap2, lists:seq(2,Cols+1)),
    % left and right
    EmptyMap = lists:foldl(fun(Row, Acc) ->
        Acc#{{Row,1}=>$|, {Row, Cols+2}=>$|}
    end, EmptyMap3, lists:seq(2,Rows+1)),
    % robots
    FullMap = lists:foldl(fun({PosX, PosY, _, _}, Acc) ->
        Acc#{{PosY+2, PosX+2} => $*}
    end, EmptyMap, Robots),
    utils:print_char_matrix(FullMap, {Rows+2, Cols+2}).


is_christmas_tree(Robots) ->
    RoboList = lists:map(fun({PosX, PosY, _, _}) -> {{PosY, PosX}, true} end, Robots),
    RoboMap = maps:from_list(RoboList),
    is_christmas_tree(RoboList, RoboMap).

is_christmas_tree([],                           _  ) -> false;
is_christmas_tree([{{Row,Col}, true}|RoboList], Map) ->
    Is = lists:all(fun(Index) -> maps:get(Index, Map, false) end, [
                       {Row+1,Col-1}, {Row+1,Col}, {Row+1,Col+1},
        {Row+2,Col-2}, {Row+2,Col-1}, {Row+2,Col}, {Row+2,Col+1}, {Row+2,Col+2}
    ]),
    case Is of
        true  -> true;
        false -> is_christmas_tree(RoboList, Map)
    end.


find_christmas_tree(Robots, Dimensions, Step) ->
    case is_christmas_tree(Robots) of
        true ->
            print_robots(Robots, Dimensions),
            Step;
        false ->
            NewRobots = move_robots(Robots, 1, Dimensions),
            find_christmas_tree(NewRobots, Dimensions, Step+1)
    end.


solve_1(FileName, Dimensions) ->
    Robots = read_inputs(FileName),
    NewRobots = move_robots(Robots, 100, Dimensions),
    count_quadrants(NewRobots, Dimensions).


solve_2(FileName, Dimensions) ->
    Robots = read_inputs(FileName),
    find_christmas_tree(Robots, Dimensions, 0).
