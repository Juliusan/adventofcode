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
        ?_assertEqual({-15,        " 23"         }, utils:get_integer("-15 23"        )),
        ?_assertEqual({0,          "-15 23"      }, utils:get_integer("0-15 23"       )),
        ?_assertEqual({15,         "anythingelse"}, utils:get_integer("15anythingelse")),
        ?_assertEqual({15,         ""            }, utils:get_integer("15"            )),
        ?_assertEqual({1234567890, ""            }, utils:get_integer("1234567890"    )),
        ?_assertThrow(_,                            utils:get_integer("+15 23"        ))
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


get_char_matrix_test_() ->
    [
        ?_assertEqual({#{},                              {0, 0}}, utils:get_char_matrix([])),
        ?_assertEqual({#{{1,1}=>$a},                     {1, 1}}, utils:get_char_matrix(["a"])),
        ?_assertEqual({#{{1,1}=>$a,{1,2}=>$b,{1,3}=>$c}, {1, 3}}, utils:get_char_matrix(["abc"])),
        ?_assertEqual({#{{1,1}=>$a,{2,1}=>$b},           {2, 1}}, utils:get_char_matrix(["a", "b"])),
        ?_assertEqual({#{{1,1}=>$a,{1,2}=>$b,{1,3}=>$c,
                         {2,1}=>$d,{2,2}=>$e,{2,3}=>$f}, {2, 3}}, utils:get_char_matrix(["abc","def"])),
        ?_assertEqual({#{{1,1}=>$a,{1,2}=>$b,{1,3}=>$c,
                         {2,1}=>$d,{2,2}=>$e,{2,3}=>$f,
                         {3,1}=>$g,{3,2}=>$h,{3,3}=>$i}, {3, 3}}, utils:get_char_matrix(["abc","def","ghi"]))
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


foldl_pairs_test_() ->
    [
        ?_assertEqual(ok,                                    utils:foldl_pairs(fun(_,  _,  _) -> fail end,         ok, [       ])),
        ?_assertEqual(ok,                                    utils:foldl_pairs(fun(_,  _,  _) -> fail end,         ok, [1      ])),
        ?_assertEqual([                              {1,2}], utils:foldl_pairs(fun(E1, E2, A) -> [{E1, E2}|A] end, [], [1,2    ])),
        ?_assertEqual([            {2,3},      {1,3},{1,2}], utils:foldl_pairs(fun(E1, E2, A) -> [{E1, E2}|A] end, [], [1,2,3  ])),
        ?_assertEqual([{3,4},{2,4},{2,3},{1,4},{1,3},{1,2}], utils:foldl_pairs(fun(E1, E2, A) -> [{E1, E2}|A] end, [], [1,2,3,4]))
    ].


list_map_sum_test_() ->
    [
        ?_assertEqual(0,  utils:list_map_sum(fun(A) -> A-$0 end, [              ])),
        ?_assertEqual(1,  utils:list_map_sum(fun(A) -> A-$0 end, [$1            ])),
        ?_assertEqual(10, utils:list_map_sum(fun(A) -> A-$0 end, [$1, $2, $3, $4]))
    ].


list_foldl_sum_test_() ->
    [
        ?_assertEqual({0, 10}, utils:list_foldl_sum(fun(A, B) -> {B*(A-$0), B-1} end, 10, [              ])),
        ?_assertEqual({10, 9}, utils:list_foldl_sum(fun(A, B) -> {B*(A-$0), B-1} end, 10, [$1            ])),
        ?_assertEqual({80, 6}, utils:list_foldl_sum(fun(A, B) -> {B*(A-$0), B-1} end, 10, [$1, $2, $3, $4]))
    ].


map_map_sum_test_() ->
    [
        ?_assertEqual(0,  utils:map_map_sum(fun(K, V) -> K*V end, #{})),
        ?_assertEqual(10, utils:map_map_sum(fun(K, V) -> K*V end, #{1 => 10})),
        ?_assertEqual(80, utils:map_map_sum(fun(K, V) -> K*V end, #{1 => 10, 2 => 9, 3 => 8, 4 => 7}))
    ].


matrix_index_of_test_() ->
    [
        ?_assertEqual(undefined, utils:matrix_index_of(a, #{}          )),
        ?_assertEqual({1,1},     utils:matrix_index_of(a, #{{1,1}=>a,{1,2}=>b,{2,1}=>c,{2,2}=>d})),
        ?_assertEqual({1,2},     utils:matrix_index_of(b, #{{1,1}=>a,{1,2}=>b,{2,1}=>c,{2,2}=>d})),
        ?_assertEqual({2,1},     utils:matrix_index_of(c, #{{1,1}=>a,{1,2}=>b,{2,1}=>c,{2,2}=>d})),
        ?_assertEqual({2,2},     utils:matrix_index_of(d, #{{1,1}=>a,{1,2}=>b,{2,1}=>c,{2,2}=>d})),
        ?_assertEqual(undefined, utils:matrix_index_of(e, #{{1,1}=>a,{1,2}=>b,{2,1}=>c,{2,2}=>d}))
    ].


matrix_is_valid_index_test_() ->
    [
        ?_assertEqual(true,  utils:matrix_is_valid_index({2,3}, {8,8})),
        ?_assertEqual(true,  utils:matrix_is_valid_index({1,1}, {8,8})),
        ?_assertEqual(true,  utils:matrix_is_valid_index({1,5}, {8,8})),
        ?_assertEqual(true,  utils:matrix_is_valid_index({1,8}, {8,8})),
        ?_assertEqual(true,  utils:matrix_is_valid_index({5,8}, {8,8})),
        ?_assertEqual(true,  utils:matrix_is_valid_index({8,8}, {8,8})),
        ?_assertEqual(true,  utils:matrix_is_valid_index({8,5}, {8,8})),
        ?_assertEqual(true,  utils:matrix_is_valid_index({8,1}, {8,8})),
        ?_assertEqual(true,  utils:matrix_is_valid_index({5,1}, {8,8})),
        ?_assertEqual(false, utils:matrix_is_valid_index({0,0}, {8,8})),
        ?_assertEqual(false, utils:matrix_is_valid_index({0,5}, {8,8})),
        ?_assertEqual(false, utils:matrix_is_valid_index({0,9}, {8,8})),
        ?_assertEqual(false, utils:matrix_is_valid_index({5,9}, {8,8})),
        ?_assertEqual(false, utils:matrix_is_valid_index({9,9}, {8,8})),
        ?_assertEqual(false, utils:matrix_is_valid_index({9,5}, {8,8})),
        ?_assertEqual(false, utils:matrix_is_valid_index({9,0}, {8,8})),
        ?_assertEqual(false, utils:matrix_is_valid_index({5,0}, {8,8}))
    ].


matrix_next_index_test_() ->
    [
        ?_assertEqual({1,3},     utils:matrix_next_index({2,3}, up,    {8,8})),
        ?_assertEqual({2,4},     utils:matrix_next_index({2,3}, right, {8,8})),
        ?_assertEqual({3,3},     utils:matrix_next_index({2,3}, down,  {8,8})),
        ?_assertEqual({2,2},     utils:matrix_next_index({2,3}, left,  {8,8})),
        ?_assertEqual(undefined, utils:matrix_next_index({1,1}, up,    {8,8})),
        ?_assertEqual({1,2},     utils:matrix_next_index({1,1}, right, {8,8})),
        ?_assertEqual({2,1},     utils:matrix_next_index({1,1}, down,  {8,8})),
        ?_assertEqual(undefined, utils:matrix_next_index({1,1}, left,  {8,8})),
        ?_assertEqual(undefined, utils:matrix_next_index({1,5}, up,    {8,8})),
        ?_assertEqual({1,6},     utils:matrix_next_index({1,5}, right, {8,8})),
        ?_assertEqual({2,5},     utils:matrix_next_index({1,5}, down,  {8,8})),
        ?_assertEqual({1,4},     utils:matrix_next_index({1,5}, left,  {8,8})),
        ?_assertEqual(undefined, utils:matrix_next_index({1,8}, up,    {8,8})),
        ?_assertEqual(undefined, utils:matrix_next_index({1,8}, right, {8,8})),
        ?_assertEqual({2,8},     utils:matrix_next_index({1,8}, down,  {8,8})),
        ?_assertEqual({1,7},     utils:matrix_next_index({1,8}, left,  {8,8})),
        ?_assertEqual({4,8},     utils:matrix_next_index({5,8}, up,    {8,8})),
        ?_assertEqual(undefined, utils:matrix_next_index({5,8}, right, {8,8})),
        ?_assertEqual({6,8},     utils:matrix_next_index({5,8}, down,  {8,8})),
        ?_assertEqual({5,7},     utils:matrix_next_index({5,8}, left,  {8,8})),
        ?_assertEqual({7,8},     utils:matrix_next_index({8,8}, up,    {8,8})),
        ?_assertEqual(undefined, utils:matrix_next_index({8,8}, right, {8,8})),
        ?_assertEqual(undefined, utils:matrix_next_index({8,8}, down,  {8,8})),
        ?_assertEqual({8,7},     utils:matrix_next_index({8,8}, left,  {8,8})),
        ?_assertEqual({7,5},     utils:matrix_next_index({8,5}, up,    {8,8})),
        ?_assertEqual({8,6},     utils:matrix_next_index({8,5}, right, {8,8})),
        ?_assertEqual(undefined, utils:matrix_next_index({8,5}, down,  {8,8})),
        ?_assertEqual({8,4},     utils:matrix_next_index({8,5}, left,  {8,8})),
        ?_assertEqual({7,1},     utils:matrix_next_index({8,1}, up,    {8,8})),
        ?_assertEqual({8,2},     utils:matrix_next_index({8,1}, right, {8,8})),
        ?_assertEqual(undefined, utils:matrix_next_index({8,1}, down,  {8,8})),
        ?_assertEqual(undefined, utils:matrix_next_index({8,1}, left,  {8,8})),
        ?_assertEqual({4,1},     utils:matrix_next_index({5,1}, up,    {8,8})),
        ?_assertEqual({5,2},     utils:matrix_next_index({5,1}, right, {8,8})),
        ?_assertEqual({6,1},     utils:matrix_next_index({5,1}, down,  {8,8})),
        ?_assertEqual(undefined, utils:matrix_next_index({5,1}, left,  {8,8}))
    ].


matrix_foldl_test_() ->
    [
        ?_assertEqual(ok,      utils:matrix_foldl(fun(_,    _,_    ) -> fail             end, ok,     #{},                                    {0, 0})),
        ?_assertEqual(29,      utils:matrix_foldl(fun({R,_},V,A    ) -> A+R*V            end, 0,      #{{1,1}=>2,{1,2}=>3,{2,1}=>5,{2,2}=>7}, {2, 2})),
        ?_assertEqual(27,      utils:matrix_foldl(fun({_,C},V,A    ) -> A+C*V            end, 0,      #{{1,1}=>2,{1,2}=>3,{2,1}=>5,{2,2}=>7}, {2, 2})),
        ?_assertEqual({156,5}, utils:matrix_foldl(fun({R,C},V,{A,M}) -> {A+M*C*R*V, M+1} end, {0, 1}, #{{1,1}=>2,{1,2}=>3,{2,1}=>5,{2,2}=>7}, {2, 2}))
    ].


integer_digit_count_test_() ->
    [
        ?_assertEqual(1,  utils:integer_digit_count(                                        1)),
        ?_assertEqual(1,  utils:integer_digit_count(                                        2)),
        ?_assertEqual(1,  utils:integer_digit_count(                                        9)),
        ?_assertEqual(2,  utils:integer_digit_count(                                       10)),
        ?_assertEqual(2,  utils:integer_digit_count(                                       28)),
        ?_assertEqual(2,  utils:integer_digit_count(                                       98)),
        ?_assertEqual(2,  utils:integer_digit_count(                                       99)),
        ?_assertEqual(3,  utils:integer_digit_count(                                      100)),
        ?_assertEqual(3,  utils:integer_digit_count(                                      999)),
        ?_assertEqual(4,  utils:integer_digit_count(                                    1_000)),
        ?_assertEqual(15, utils:integer_digit_count(                      999_999_999_999_999)),
        ?_assertEqual(16, utils:integer_digit_count(                    1_000_000_000_000_000)),
        ?_assertEqual(30, utils:integer_digit_count(  999_999_999_999_999_999_999_999_999_999)),
        ?_assertEqual(31, utils:integer_digit_count(1_000_000_000_000_000_000_000_000_000_000))
    ].


integer_10_pow_test_() ->
    [
        ?_assertEqual(1,                                         utils:integer_10_pow( 0)),
        ?_assertEqual(10,                                        utils:integer_10_pow( 1)),
        ?_assertEqual(100,                                       utils:integer_10_pow( 2)),
        ?_assertEqual(1_000,                                     utils:integer_10_pow( 3)),
        ?_assertEqual(10_000_000_000,                            utils:integer_10_pow(10)),
        ?_assertEqual(100_000_000_000_000_000_000,               utils:integer_10_pow(20)),
        ?_assertEqual(1_000_000_000_000_000_000_000_000_000_000, utils:integer_10_pow(30)),
        ?_assert(true)
    ].


concat_integers_test_() ->
    [
        ?_assertEqual(10,                             utils:concat_integers(1,            0      )),
        ?_assertEqual(321,                            utils:concat_integers(3,            21     )),
        ?_assertEqual(321,                            utils:concat_integers(32,           1      )),
        ?_assertEqual(999,                            utils:concat_integers(9,            99     )),
        ?_assertEqual(999,                            utils:concat_integers(99,           9      )),
        ?_assertEqual(12345678901234567890,           utils:concat_integers(1234567890123,4567890          )),
        ?_assertEqual(123456789012345678901234567890, utils:concat_integers(1,23456789012345678901234567890))
    ].


split_integer_test_() ->
    [
        ?_assertEqual({1,            0      },           utils:split_integer(10,                            1 )),
        ?_assertEqual({32,           1      },           utils:split_integer(321,                           1 )),
        ?_assertEqual({3,            21     },           utils:split_integer(321,                           2 )),
        ?_assertEqual({99,           9      },           utils:split_integer(999,                           1 )),
        ?_assertEqual({9,            99     },           utils:split_integer(999,                           2 )),
        ?_assertEqual({1234567890123,4567890},           utils:split_integer(12345678901234567890,          7 )),
        ?_assertEqual({12345678901234567890123456789,0}, utils:split_integer(123456789012345678901234567890,1 )),
        ?_assertEqual({1,23456789012345678901234567890}, utils:split_integer(123456789012345678901234567890,29))
    ].


euclidean_div_test_() ->
    [
        ?_assertEqual( 2, utils:euclidean_div( 7,  3)),
        ?_assertEqual(-2, utils:euclidean_div( 7, -3)),
        ?_assertEqual(-3, utils:euclidean_div(-7,  3)),
        ?_assertEqual( 3, utils:euclidean_div(-7, -3)),
        ?_assertEqual( 2, utils:euclidean_div( 8,  3)),
        ?_assertEqual(-2, utils:euclidean_div( 8, -3)),
        ?_assertEqual(-3, utils:euclidean_div(-8,  3)),
        ?_assertEqual( 3, utils:euclidean_div(-8, -3)),
        ?_assertEqual( 3, utils:euclidean_div( 9,  3)),
        ?_assertEqual(-3, utils:euclidean_div( 9, -3)),
        ?_assertEqual(-3, utils:euclidean_div(-9,  3)),
        ?_assertEqual( 3, utils:euclidean_div(-9, -3))
    ].


euclidean_rem_test_() ->
    [
        ?_assertEqual(1, utils:euclidean_rem( 7,  3)),
        ?_assertEqual(1, utils:euclidean_rem( 7, -3)),
        ?_assertEqual(2, utils:euclidean_rem(-7,  3)),
        ?_assertEqual(2, utils:euclidean_rem(-7, -3)),
        ?_assertEqual(2, utils:euclidean_rem( 8,  3)),
        ?_assertEqual(2, utils:euclidean_rem( 8, -3)),
        ?_assertEqual(1, utils:euclidean_rem(-8,  3)),
        ?_assertEqual(1, utils:euclidean_rem(-8, -3)),
        ?_assertEqual(0, utils:euclidean_rem( 9,  3)),
        ?_assertEqual(0, utils:euclidean_rem( 9, -3)),
        ?_assertEqual(0, utils:euclidean_rem(-9,  3)),
        ?_assertEqual(0, utils:euclidean_rem(-9, -3))
    ].