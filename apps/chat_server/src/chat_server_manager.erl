-module(chat_server_manager).
-export([start/0, loop/2]).

-define(ReValidName, "^[a-zA-Z0-9]{3,}+$").
-define(RePatternUserlist, "<<user,list>>.*").
-define(RePatternWhoami, "<<user,whoami>>.*").
-define(RePatternSetname, "<<user,setname>>.+").
-define(RePatternMsg, "<<msg,[a-zA-Z0-9]{3,}+>>+").

start() ->
  chat_server_socket:start(?MODULE, 7000, {?MODULE, loop}).

loop(Socket, Pid) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Data} ->
      io:format("~s", [Data]),
      case manage_cmd_get_resp(Data, Pid) of
        {ok, Resp} -> gen_tcp:send(Socket, Resp);
        _ -> ok
      end,
      loop(Socket, Pid);
    {error, closed} ->
      client_disconnection(Pid),
      ok
  end.

manage_cmd_get_resp(Data, Pid) ->
  Str = binary_to_list(string:chomp(Data)),
  Pattern_user_setname = pattern_user_setname(Data),
  Pattern_user_list_res = pattern_user_list(Data),
  Pattern_user_whoami = pattern_user_whoami(Data),
  Pattern_msg = pattern_msg(Data),
  if
    Pattern_user_setname ->
      chat_server_commands:cmd_user_setname(Pid, Data);
    Pattern_user_list_res ->
      chat_server_commands:cmd_user_list();
    Pattern_user_whoami ->
      chat_server_commands:cmd_user_whoami(Pid);
    Pattern_msg ->
      chat_server_commands:cmd_msg(Pid, Data);
    % "<<room,list>>" ++ _ ->
    %   ok;
    % "<<room,userlist," ++ RoomNameList ++ ">>" ++ _ ->
    %   ok;
    % "<<room,userjoin," ++ RoomNameJoin ++ ">>" ++ _ ->
    %   ok;
    % "<<room,userleave," ++ RoomNameLeave ++ ">>" ++ _ ->
    %   ok;
    % "<<room,create," ++ RoomNameCreate ++ ">>" ++ _ ->
    %   ok;
    % "<<room,delete," ++ RoomNameDelete ++ ">>" ++ _ ->
    %   ok;
    % "<<room,msg," ++ RoomNameMsg ++ ">>" ++ RoomMsg ->
    %   ok;
    true ->
      chat_server_commands:cmd_default(Str)
  end.

pattern_user_setname(Data) ->
  case re:run(Data, ?RePatternSetname) of
    {match, _} -> true;
    nomatch -> false
  end.

pattern_user_list(Data) ->
  case re:run(Data, ?RePatternUserlist) of
    {match, _} -> true;
    nomatch -> false
  end.

pattern_user_whoami(Data) ->
  case re:run(Data, ?RePatternWhoami) of
    {match, _} -> true;
    nomatch -> false
  end.

pattern_msg(Data) ->
  case re:run(Data, ?RePatternMsg) of
    {match, _} -> true;
    nomatch -> false
  end.

client_disconnection(Pid) ->
  chat_server_users:delete(Pid),
  io:format("error - closed ~w~n", [Pid]).
  
