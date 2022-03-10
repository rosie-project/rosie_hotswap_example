%%%-------------------------------------------------------------------
%% @doc rosie_hotswap_example top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(meda_robot_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 0,
        period => 1
    },
    TEMP_SERVER = #{
        id => temp_sensor,
        start => {temp_sensor, start_link, []}
    },
    WIND_SERVER = #{
        id => wind_sensor,
        start => {wind_sensor, start_link, []}
    },
    LOCATION_SERVER = #{
        id => location_sensor,
        start => {location_sensor, start_link, []}
    },
    MEDA_SERVER = #{
        id => meda_robot,
        start => {meda_robot, start_link, []}
    },
    ChildSpecs = [TEMP_SERVER, WIND_SERVER, LOCATION_SERVER, MEDA_SERVER],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
