%%%
%%% Unit tests for utils.
%%%
-module(utils_tests).
-include_lib("eunit/include/eunit.hrl").


% 74> c(utils_tests).
% {ok,utils_tests}
% 75> utils_tests:test().
%   All 11 tests passed.


get_integer_test_() ->
    [
        ?_assertEqual({15,         " 23"         }, utils:get_integer("15 23"         )),
        ?_assertEqual({15,         "anythingelse"}, utils:get_integer("15anythingelse")),
        ?_assertEqual({15,         ""            }, utils:get_integer("15"            )),
        ?_assertEqual({1234567890, ""            }, utils:get_integer("1234567890"    ))
    ].


get_integer_list_test_() ->
    [
        ?_assertEqual([],                 utils:get_integer_list(""                  )),
        ?_assertEqual([23],               utils:get_integer_list("23"                )),
        ?_assertEqual([23],               utils:get_integer_list("23   "             )),
        ?_assertEqual([23],               utils:get_integer_list("    23"            )),
        ?_assertEqual([23],               utils:get_integer_list("    23   "         )),
        ?_assertEqual([15, 23],           utils:get_integer_list("15 23"             )),
        ?_assertEqual([15, 2321, 12, 24], utils:get_integer_list("15     2321 12  24")),

        ?_assertEqual([15, 23],           utils:get_integer_list("15,23"          , "," )),
        ?_assertEqual([15, 2321, 12, 24], utils:get_integer_list("15,2321,,12,24,", "," )),
        ?_assertEqual([15, 2321, 2014],   utils:get_integer_list("15, 2321, 2014",  ", "))
    ].


drop_trailing_new_line_test_() ->
    [
        ?_assertEqual("",           utils:drop_trailing_new_line("\n"          )),
        ?_assertEqual("\n",         utils:drop_trailing_new_line("\n\n"        )),
        ?_assertEqual("abcdef",     utils:drop_trailing_new_line("abcdef\n"    )),
        ?_assertEqual("abc\ndef",   utils:drop_trailing_new_line("abc\ndef\n"  )),
        ?_assertEqual("ab\ncd\nef", utils:drop_trailing_new_line("ab\ncd\nef\n"))
    ].


count_elems_sorted_test_() ->
    [
        ?_assertEqual({3, [16, 30, 123]}, utils:count_elems_sorted(15, [1, 2, 3, 15, 15, 15, 16, 30, 123])),
        ?_assertEqual({0, [16, 30, 123]}, utils:count_elems_sorted(15, [1, 2, 3,             16, 30, 123])),
        ?_assertEqual({1, [16, 30, 123]}, utils:count_elems_sorted(15, [         15,         16, 30, 123])),
        ?_assertEqual({3, [16, 30, 123]}, utils:count_elems_sorted(15, [         15, 15, 15, 16, 30, 123]))
    ].


count_elems_start_test_() ->
    [
        ?_assertEqual({1, [16, 30, 123]}, utils:count_elems_start([15,         16, 30, 123])),
        ?_assertEqual({3, [16, 30, 123]}, utils:count_elems_start([15, 15, 15, 16, 30, 123]))
    ].


is_decreasing_test_() ->
    [
        ?_assertEqual(true,  utils:is_decreasing([],                   1)),
        ?_assertEqual(true,  utils:is_decreasing([20],                 1)),
        ?_assertEqual(true,  utils:is_decreasing([20, 19, 18, 17, 16], 1)),
        ?_assertEqual(false, utils:is_decreasing([20, 19, 18, 15, 12], 1)),
        ?_assertEqual(true,  utils:is_decreasing([20, 19, 18, 17, 16], 3)),
        ?_assertEqual(true,  utils:is_decreasing([20, 19, 18, 15, 12], 3)),
        ?_assertEqual(false, utils:is_decreasing([20, 15, 14, 13, 12], 3)),
        ?_assertEqual(false, utils:is_decreasing([20, 21, 19, 17, 15], 3))
    ].


is_increasing_test_() ->
    [
        ?_assertEqual(true,  utils:is_increasing([],                   1)),
        ?_assertEqual(true,  utils:is_increasing([20],                 1)),
        ?_assertEqual(true,  utils:is_increasing([20, 21, 22, 23, 24], 1)),
        ?_assertEqual(false, utils:is_increasing([20, 21, 22, 25, 28], 1)),
        ?_assertEqual(true,  utils:is_increasing([20, 21, 22, 23, 24], 3)),
        ?_assertEqual(true,  utils:is_increasing([20, 21, 22, 25, 28], 3)),
        ?_assertEqual(false, utils:is_increasing([20, 25, 26, 27, 28], 3)),
        ?_assertEqual(false, utils:is_increasing([20, 19, 21, 23, 25], 3))
    ].


transpose_test_() ->
    [
        ?_assertEqual([],                        utils:transpose([])),
        ?_assertEqual([[1]  ],                   utils:transpose([[1]  ])),
        ?_assertEqual([[1],    [2]    ],         utils:transpose([[1,2]])),
        ?_assertEqual([[1,2]],                   utils:transpose([[1],    [2]    ])),
        ?_assertEqual([[1,4],  [2,5],  [3,6]  ], utils:transpose([[1,2,3],[4,5,6]])),
        ?_assertEqual([[1,2,3],[4,5,6]],         utils:transpose([[1,4],  [2,5],  [3,6]  ])),
        ?_assertEqual([[1,4,7],[2,5,8],[3,6,9]], utils:transpose([[1,2,3],[4,5,6],[7,8,9]]))
    ].


diagonals_f_test_() ->
    [
        ?_assertEqual([],                            utils:diagonals_f([])),
        ?_assertEqual([[1]],                         utils:diagonals_f([[1]  ])),
        ?_assertEqual([[2],[1]],                     utils:diagonals_f([[1,2]])),
        ?_assertEqual([[2],[1]],                     utils:diagonals_f([[1],    [2]    ])),
        ?_assertEqual([[4],[3,2],[1]],               utils:diagonals_f([[1,2],  [3,4]  ])),
        ?_assertEqual([[6],[5,3],[4,2],[1]],         utils:diagonals_f([[1,2,3],[4,5,6]])),
        ?_assertEqual([[6],[5,4],[3,2],[1]],         utils:diagonals_f([[1,2],  [3,4],  [5,6]  ])),
        ?_assertEqual([[9],[8,6],[7,5,3],[4,2],[1]], utils:diagonals_f([[1,2,3],[4,5,6],[7,8,9]]))
    ].


diagonals_b_test_() ->
    [
        ?_assertEqual([],                            utils:diagonals_b([])),
        ?_assertEqual([[1]],                         utils:diagonals_b([[1]  ])),
        ?_assertEqual([[1],[2]],                     utils:diagonals_b([[1,2]])),
        ?_assertEqual([[2],[1]],                     utils:diagonals_b([[1],    [2]    ])),
        ?_assertEqual([[3],[4,1],[2]],               utils:diagonals_b([[1,2],  [3,4]  ])),
        ?_assertEqual([[4],[5,1],[6,2],[3]],         utils:diagonals_b([[1,2,3],[4,5,6]])),
        ?_assertEqual([[5],[6,3],[4,1],[2]],         utils:diagonals_b([[1,2],  [3,4],  [5,6]  ])),
        ?_assertEqual([[7],[8,4],[9,5,1],[6,2],[3]], utils:diagonals_b([[1,2,3],[4,5,6],[7,8,9]]))
    ].


middle_test_() ->
    [
        ?_assertEqual([1],   utils:middle([1]          )),
        ?_assertEqual([1,2], utils:middle([1,2]        )),
        ?_assertEqual([2],   utils:middle([1,2,3]      )),
        ?_assertEqual([2,3], utils:middle([1,2,3,4]    )),
        ?_assertEqual([n],   utils:middle([a,6,n,u,9]  )),
        ?_assertEqual([n,5], utils:middle([a,6,n,5,u,9]))
    ].


middle_single_test_() ->
    [
        ?_assertEqual(1, utils:middle_single([1]          )),
        ?_assertEqual(2, utils:middle_single([1,2,3]      )),
        ?_assertEqual(n, utils:middle_single([a,6,n,u,9]  )),
        ?_assertError(_, utils:middle_single([1,2]        )),
        ?_assertError(_, utils:middle_single([1,2,3,4]    )),
        ?_assertError(_, utils:middle_single([a,6,n,5,u,9]))
    ].