-module(temp_sensor).

-export([start_link/0, read/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2]).

-include_lib("sensor_msgs/src/_rosie/sensor_msgs_temperature_msg.hrl").

-record(state, {pressure_csv}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

read() ->
    gen_server:call(?MODULE, read).

init(_) ->
    CSV = robot_utils:open_csv("data/WE__0016___________CAL_PS__________________P02.CSV"),
    {ok, #state{pressure_csv = CSV}}.

handle_call(read, _, #state{pressure_csv = W} = S) ->
    L = robot_utils:read_line(W),
    % io:format("Temperature = ~p\n",[maps:get("THERMOCAP1_TEMP",L)]),
    MSG = #sensor_msgs_temperature{
        header = #std_msgs_header{
            stamp = #builtin_interfaces_time{},
            frame_id = "robot_frame"
        },
        temperature = robot_utils:force_to_float(maps:get("THERMOCAP1_TEMP", L)),
        variance = robot_utils:force_to_float(maps:get("THERMOCAP1_TEMP_UNCERTAINTY", L))
    },

    % FIX
    {reply, MSG, S};
    %
    %BUG
    % Unstable = MSG#sensor_msgs_temperature.temperature * (rand:uniform() - 0.5) * 50,
    % {reply, MSG#sensor_msgs_temperature{temperature = Unstable}, S};
%
handle_call(_, _, S) ->
    {reply, ok, S}.

handle_cast(_, S) ->
    {noreply, S}.
