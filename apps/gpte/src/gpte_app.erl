%%%-------------------------------------------------------------------
%% @doc gpte public API
%% @end
%%%-------------------------------------------------------------------

-module(gpte_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    gpte_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
