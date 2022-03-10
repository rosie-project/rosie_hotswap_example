-module(meda_robot).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-include_lib("rosie_dds/include/dds_types.hrl").


-include("src/_rosie/meda_robot_wind_data_msg.hrl").

-include_lib("geometry_msgs/src/_rosie/geometry_msgs_twist_msg.hrl").
-include_lib("geometry_msgs/src/_rosie/geometry_msgs_pose_stamped_msg.hrl").

-include_lib("tf2_msgs/src/_rosie/tf2_msgs_t_f_message_msg.hrl").
-include_lib("visualization_msgs/src/_rosie/visualization_msgs_marker_msg.hrl").

-record(state, {
    ros_node, 
    tf_static_Pub, 
    tf_dinamic_Pub, 
    % marker_pub, 
    wind_boom1_pub, 
    wind_boom2_pub, 
    temp1_pub
}).

-define(GLOBAL_NAME, "meda_robot").
-define(POLLING_PERIOD, 1000).
% keep at 500 to match rover sample period
-define(PUB_PERIOD, 100).

start_link() ->
    gen_server:start_link({global, ?GLOBAL_NAME}, ?MODULE, #state{}, []).

init(_) ->
    Node = ros_context:create_node(?GLOBAL_NAME),
    TF_dinamic_Pub = ros_node:create_publisher(Node, tf2_msgs_t_f_message_msg, "tf", #qos_profile{
        durability = ?TRANSIENT_LOCAL_DURABILITY_QOS
    }),
    TF_static_Pub = ros_node:create_publisher(
        Node, tf2_msgs_t_f_message_msg, "tf_static", #qos_profile{
            durability = ?TRANSIENT_LOCAL_DURABILITY_QOS
        }
    ),
    % Marker_Pub = ros_node:create_publisher(
    %     Node, visualization_msgs_marker_msg, "visualization_marker"
    % ),
    Wind_boom1_Pub = ros_node:create_publisher(Node, geometry_msgs_pose_stamped_msg, "wind_boom_1"),
    Wind_boom2_Pub = ros_node:create_publisher(Node, geometry_msgs_pose_stamped_msg, "wind_boom_2"),
    Temp1_pub = ros_node:create_publisher(Node, sensor_msgs_temperature_msg, "thermocap_1"),

    self() ! wait_for_rviz,

    {ok, #state{
        ros_node = Node,
        tf_dinamic_Pub = TF_dinamic_Pub,
        tf_static_Pub = TF_static_Pub,
        % marker_pub = Marker_Pub,
        wind_boom1_pub = Wind_boom1_Pub,
        wind_boom2_pub = Wind_boom2_Pub,
        temp1_pub = Temp1_pub
    }}.

handle_call(_, _, S) ->
    {reply, ok, S}.

handle_cast(_, S) ->
    {noreply, S}.

handle_info(
    publish,
    #state{
        wind_boom1_pub = Wind_1P,
        wind_boom2_pub = Wind_2P,
        tf_dinamic_Pub = TF_DIN_PUB,
        % marker_pub = Marker_Pub,
        temp1_pub = Temp1_pub
    } = S
) ->
    case {location_sensor:read(), wind_sensor:read(), temp_sensor:read()} of
        {eof, _, _} ->
            io:format("Reached EOF\n"),
            {noreply, S};
        {Pos, W, T} ->
            ros_publisher:publish(Temp1_pub, T),
            ros_publisher:publish(Wind_1P, wind_to_pose(W#meda_robot_wind_data.boom1)),
            ros_publisher:publish(Wind_2P, wind_to_pose(W#meda_robot_wind_data.boom2)),
            ros_publisher:publish(TF_DIN_PUB, tf_robot_frame_to_mars(Pos)),
            %ros_publisher:publish(Marker_Pub, marker_info(Pos, ?MODIFY)),
            erlang:send_after(?PUB_PERIOD, self(), publish),
            {noreply, S}
    end;
handle_info(wait_for_rviz, #state{tf_static_Pub = TF_Pub} = S) ->
    case (ros_publisher:get_subscription_count(TF_Pub) >= 1) of
        true ->
            ros_publisher:publish(TF_Pub, tf_mars_to_map()),
            %ros_publisher:publish(Marker_Pub, marker_info()),
            self() ! publish;
        false ->
            io:format("Waiting for a remote display...\n"),
            erlang:send_after(?POLLING_PERIOD, self(), wait_for_rviz)
    end,
    {noreply, S};
handle_info(_, S) ->
    {noreply, S}.

wind_to_pose(#meda_robot_boom{horizontal_wind_speed = _H_SPEED, horizontal_wind_direction = H_DIR}) ->
    #geometry_msgs_pose_stamped{
        header = #std_msgs_header{
            stamp = #builtin_interfaces_time{},
            frame_id = "robot_frame"
        },
        pose = #geometry_msgs_pose{
            position = #geometry_msgs_point{x = 0.0, y = 0.0, z = 2.0},
            % FIX
            orientation = robot_utils:get_quaternion_from_euler(0.0, 0.0, H_DIR)
            % BUG
            % orientation = robot_utils:get_quaternion_from_euler(
            %    0.0, (rand:uniform() - 0.5) * 90, H_DIR
            % )
        }
    }.

% wind_to_velocity(#meda_msgs_wind_data{
%     time = #meda_msgs_time{sclk = _SCLK, lmst = _LMST, ltst = _LTST},
%     boom1 = #meda_msgs_boom{horizontal_wind_speed = H_SPEED1, horizontal_wind_direction = H_DIR1},
%     boom2 = #meda_msgs_boom{horizontal_wind_speed = H_SPEED2, horizontal_wind_direction = H_DIR2}
% }) ->
%     % SPEED = (H_SPEED1 + H_SPEED2) / 2,
%     % ANGLE = ((H_DIR1 + H_DIR2) / 2)  * math:pi() / 180.0,
%     SPEED = H_SPEED1,
%     ANGLE = H_DIR1 * math:pi() / 180.0,
%     %io:format("SPEED: ~p, ANGLE: ~p°\n",[SPEED,((H_DIR1 + H_DIR2) / 2)]),
%     #geometry_msgs_twist{
%         linear = #geometry_msgs_vector3{
%             x = SPEED * math:cos(ANGLE),
%             z = 0.0,
%             y = SPEED * math:sin(ANGLE)
%         }
%     }.
tf_mars_to_map() ->
    #tf2_msgs_t_f_message{
        transforms = [
            #geometry_msgs_transform_stamped{
                header = #std_msgs_header{
                    stamp = #builtin_interfaces_time{},
                    frame_id = "map"
                },
                child_frame_id = "mars_space",
                transform = #geometry_msgs_transform{
                    translation = #geometry_msgs_vector3{x = -31.0, y = 8.0, z = 0.40},
                    rotation = robot_utils:get_quaternion_from_euler(0.0, 0.0, 0.0)
                }
            }
        ]
    }.
tf_robot_frame_to_mars({#geometry_msgs_vector3{x = _X, y = _Y, z = _Z} = Pos, _Speed, Orientation}) ->
    #tf2_msgs_t_f_message{
        transforms = [
            #geometry_msgs_transform_stamped{
                header = #std_msgs_header{
                    stamp = #builtin_interfaces_time{},
                    frame_id = "mars_space"
                },
                child_frame_id = "robot_frame",
                transform = #geometry_msgs_transform{
                    translation = Pos,
                    rotation = Orientation
                }
            }
        ]
    }.

% marker_info() -> marker_info({#geometry_msgs_point{},0.0,#geometry_msgs_quaternion{w=1.0}}, ?ADD).
% marker_info({Position, Speed, Orientation}, Mode) ->
%     #visualization_msgs_marker{
%         header=#std_msgs_header{
%             stamp = #builtin_interfaces_time{},
%             frame_id = "robot_frame"
%         },
%         ns="",
%         id=0,
%         type=?CUBE,
%         action=Mode,
%         pose=#geometry_msgs_pose{ position = Position,
%                                     orientation = Orientation},
%         scale=#geometry_msgs_vector3{x=1.0,y=1.0,z=1.0},
%         color=#std_msgs_color_r_g_b_a{r=1.0,g=1.0,b=1.0,a=1.0},
%         lifetime=#builtin_interfaces_duration{},
%         frame_locked=false,
%         points=[],
%         colors=[],
%         texture_resource="",
%         texture=#sensor_msgs_compressed_image{},
%         uv_coordinates=[],
%         text="",
%         mesh_resource="/mnt/c/Users/Luca Succi/Desktop/perseverance.stl",
%         mesh_file=#visualization_msgs_mesh_file{},
%         mesh_use_embedded_materials=false
%     }.
