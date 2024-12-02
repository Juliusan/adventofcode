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
        ?_assertEqual([23, 15],           utils:get_integer_list("15 23"             )),
        ?_assertEqual([24, 12, 2321, 15], utils:get_integer_list("15     2321 12  24"))
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