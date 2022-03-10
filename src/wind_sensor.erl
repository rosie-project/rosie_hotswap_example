-module(wind_sensor).

-export([start_link/0, read/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2]).

-include("src/_rosie/meda_robot_wind_data_msg.hrl").

-record(state, {wind_csv}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

read() ->
    gen_server:call(?MODULE, read).

init(_) ->
    CSV = robot_utils:open_csv("data/WE__0099___________CAL_WS__________________P01.CSV"),
    {ok, #state{wind_csv = CSV}}.

handle_call(read, _, #state{wind_csv = W} = S) ->
    L = robot_utils:read_line(W),
    MSG = #meda_robot_wind_data{
        time = #meda_robot_time{
            sclk = maps:get("SCLK", L),
            lmst = maps:get("LMST", L),
            ltst = maps:get("LTST", L)
        },
        boom1 = #meda_robot_boom{
            horizontal_wind_speed = robot_utils:force_to_float(
                maps:get("BOOM1_HORIZONTAL_WIND_SPEED", L)
            ),
            horizontal_wind_direction = robot_utils:force_to_float(
                maps:get("BOOM1_WIND_DIRECTION", L)
            )
        },
        boom2 = #meda_robot_boom{
            horizontal_wind_speed = robot_utils:force_to_float(
                maps:get("BOOM2_HORIZONTAL_WIND_SPEED", L)
            ),
            horizontal_wind_direction = robot_utils:force_to_float(
                maps:get("BOOM2_WIND_DIRECTION", L)
            )
        }
    },
    {reply, MSG, S};
handle_call(_, _, S) ->
    {reply, ok, S}.

handle_cast(_, S) ->
    {noreply, S}.
