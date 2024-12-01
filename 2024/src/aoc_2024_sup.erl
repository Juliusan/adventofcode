%%%
%%% Main supervisor for the Advent of code 2024 application.
%%%
-module(aoc_2024_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).


%%% ============================================================================
%%% Public API.
%%% ============================================================================

%%
%%  Create this supervisor.
%%
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, {}).



%%% ============================================================================
%%% Callbacks for `supervisor`.
%%% ============================================================================

%%
%%  Supervisor initialization.
%%
init({}) ->
    SupFlags = #{
        strategy  => rest_for_one,
        intensity => 100,
        period    => 10
    },
    {ok, {SupFlags, []}}.
