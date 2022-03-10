-module(robot_utils).

-export([
    open_csv/1,
    read_line/1,
    close_csv/1,
    force_to_float/1,
    get_quaternion_from_euler/3
]).

-include_lib("meda_robot/include/utils.hrl").
-include_lib("geometry_msgs/src/_rosie/geometry_msgs_quaternion_msg.hrl").

open_csv(FileName) ->
    {ok, IODevice} = file:open(FileName, [read]),
    {ok, Line} = file:read_line(IODevice),
    Headers = string:split(string:trim(Line), ",", all),
    #csv{file_channel = IODevice, headers = Headers}.

read_line(#csv{file_channel = IODevice, headers = H}) ->
    case file:read_line(IODevice) of
        {ok, Line} ->
            Fields = string:split(string:trim(Line), ",", all),
            lists:foldl(
                fun({Type, Value}, Map) -> Map#{Type => Value} end, #{}, lists:zip(H, Fields)
            );
        eof ->
            file:close(IODevice),
            eof
    end.

close_csv(#csv{file_channel = IODevice}) ->
    file:close(IODevice).

force_to_float([]) ->
    0.0;
force_to_float(List) when is_list(List) ->
    IsInt = re:run(List, "^-?[0-9]+$", [{capture, none}]),
    IsFloat = re:run(List, "^-?[0-9]+\\.[0-9]+$", [{capture, none}]),
    case {IsInt, IsFloat} of
        {match, _} -> float(list_to_integer(List));
        {_, match} -> list_to_float(List)
    end.

get_quaternion_from_euler(RollDeg, PitchDeg, YawDeg) ->
    Roll = RollDeg * math:pi() / 180.0,
    Pitch = PitchDeg * math:pi() / 180.0,
    Yaw = YawDeg * math:pi() / 180.0,
    % Convert an Euler angle to a Quaternion.
    %
    % Input
    %   :param roll: The roll (rotation around x-axis) angle in radians.
    %   :param pitch: The pitch (rotation around Y-axis) angle in radians.
    %   :param Yaw: The Yaw (rotation around z-axis) angle in radians.
    %
    % Output
    %   :return Qx, QY, Qz, Qw: The orientation in Quaternion [x,Y,z,w] format
    Qx =
        math:sin(Roll / 2) * math:cos(Pitch / 2) * math:cos(Yaw / 2) -
            math:cos(Roll / 2) * math:sin(Pitch / 2) * math:sin(Yaw / 2),
    Qy =
        math:cos(Roll / 2) * math:sin(Pitch / 2) * math:cos(Yaw / 2) +
            math:sin(Roll / 2) * math:cos(Pitch / 2) * math:sin(Yaw / 2),
    Qz =
        math:cos(Roll / 2) * math:cos(Pitch / 2) * math:sin(Yaw / 2) -
            math:sin(Roll / 2) * math:sin(Pitch / 2) * math:cos(Yaw / 2),
    Qw =
        math:cos(Roll / 2) * math:cos(Pitch / 2) * math:cos(Yaw / 2) +
            math:sin(Roll / 2) * math:sin(Pitch / 2) * math:sin(Yaw / 2),
    #geometry_msgs_quaternion{x = Qx, y = Qy, z = Qz, w = Qw}.
