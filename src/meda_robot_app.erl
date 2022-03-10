%%%-------------------------------------------------------------------
%% @doc rosie_hotswap_example public API
%% @end
%%%-------------------------------------------------------------------

-module(meda_robot_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    meda_robot_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
