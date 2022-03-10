-module(location_sensor).

-export([start_link/0, read/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2]).

-include("src/_rosie/meda_robot_wind_data_msg.hrl").
-include_lib("geometry_msgs/src/_rosie/geometry_msgs_vector3_msg.hrl").
-include_lib("geometry_msgs/src/_rosie/geometry_msgs_point_msg.hrl").

-record(state, {wind_csv}).

-define(ANCILLARY_FILE, "data/ancillary_16.csv").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

read() ->
    gen_server:call(?MODULE, read).

init(_) ->
    CSV = robot_utils:open_csv(?ANCILLARY_FILE),
    {ok, #state{wind_csv = CSV}}.

handle_call(read, _, #state{wind_csv = W} = S) ->
    case robot_utils:read_line(W) of
        eof ->
            io:format("Looping location data...\n"),
            robot_utils:close_csv(W),
            CSV = robot_utils:open_csv(?ANCILLARY_FILE),
            L = robot_utils:read_line(CSV),
            {reply, line_to_record(L), #state{wind_csv = CSV}};
        L ->
            {reply, line_to_record(L), S}
    end;
handle_call(_, _, S) ->
    {reply, ok, S}.

handle_cast(_, S) ->
    {noreply, S}.

line_to_record(L) ->
    %     maps:get("SCLK",L),
    %     maps:get("LMST",L),
    %     maps:get("LTST",L)
    X = robot_utils:force_to_float(maps:get("ROVER_POSITION_X", L)),
    Y = robot_utils:force_to_float(maps:get("ROVER_POSITION_Y", L)),
    Z = robot_utils:force_to_float(maps:get("ROVER_POSITION_Z", L)),
    Speed = robot_utils:force_to_float(maps:get("ROVER_VELOCITY", L)),
    Pitch = robot_utils:force_to_float(maps:get("ROVER_PITCH", L)),
    Yaw = robot_utils:force_to_float(maps:get("ROVER_YAW", L)),
    Roll = robot_utils:force_to_float(maps:get("ROVER_ROLL", L)),

    % FIX
    Position = #geometry_msgs_vector3{x=X,y=Y,z=Z},
    % BUG
    % Z_bug = Z + (rand:uniform() - 0.5) * rand:uniform() * 2,
    % Position = #geometry_msgs_vector3{x = X, y = Y, z = Z_bug},
    % BUG
    Orientation = robot_utils:get_quaternion_from_euler(Roll, Pitch, Yaw),
    {Position, Speed, Orientation}.
