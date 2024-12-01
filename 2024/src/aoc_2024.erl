%%%
%%% @doc
%%% This module is also an OTP Application module for the Advent of code 2024 application.
%%%
-module(aoc_2024).
-behaviour(application).
-export([start/2, stop/1]).


%%% ============================================================================
%%% Callbacks for `application`.
%%% ============================================================================

%%
%%  Start the application.
%%
start(_StartType, _StartArgs) ->
    {ok, Pid} = aoc_2024_sup:start_link(),
    {ok, Pid, {}}.


%%
%%  Stop the application.
%%
stop({}) ->
    ok.
