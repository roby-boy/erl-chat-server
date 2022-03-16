-module(chat_server_manager).
-export([start/0, loop/2]).

start() ->
  chat_server_socket:start(?MODULE, 7000, {?MODULE, loop}).

loop(Socket, Pid) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Data} ->
      io:format("~s", [Data]),
      case manage_cmd_get_resp(Data, Pid) of
        {ok, Resp} -> gen_tcp:send(Socket, Resp);
        {ok, _} -> ok
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
      cmd_user_setname(Pid, Data);
    Pattern_user_list_res ->
      cmd_user_list();
    Pattern_user_whoami ->
      cmd_user_whoami(Pid);
    Pattern_msg ->
      cmd_msg(Pid, Data);
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
      cmd_default(Str)
  end.

cmd_user_list() ->
  io:format("[cmd] <<user,list>>~n",[]),
  Names = chat_server_users:getAllNames(),
  io:format("~p~n",[Names]),
  Resp = "users:" ++ string:join(Names,",") ++ "\n",
  {ok, Resp}.

cmd_user_setname(Pid, Data) ->
  io:format("[cmd] <<user,setname>>~n",[]),
  N1 = re:replace(Data, "<<user,setname>>", "", [{return,list}]),
  N2 = string:trim(N1),
  Ntest = is_valid_name(N2),
  % check if already taken
  if
    Ntest ->
      chat_server_users:putName(Pid, N2),
      {ok, "ok\n"};
    true ->
      {ok, "name is not valid; at least 3 chars of letters/numbers~n"}
  end.

cmd_user_whoami(Pid) ->
  Name = chat_server_users:getName(Pid),
  case io_lib:char_list(Name) of
    true -> {ok, Name ++ "\n"};
    false -> {ok, "name is not set yet\n"}
  end.

cmd_msg(Pid, Data) ->
  RecvText = re:replace(Data, "<<msg,", "", [{return,list}]),
  RecvTextNoRC = re:replace(RecvText, "\n", "", [{return,list}]),
  Text = re:replace(RecvTextNoRC, "[a-zA-Z0-9]{3,}+>>", "", [{return,list}]),
  Recv = re:replace(RecvTextNoRC, ">>.*", "", [{return,list}]),
  io:format("Recv:~s~n",[Recv]),
  io:format("Text:~s~n",[Text]),
  NameSendIsSet = chat_server_users:nameIsSet(Pid), % sender
  NameRecvIsSet = chat_server_users:nameIsSetByName(Recv), % receiver
  if
    not NameSendIsSet ->
      {ok, "name send not set\n"};
    not NameRecvIsSet ->
      {ok, "name recv not set\n"};
    true ->
      SocketRecv = chat_server_users:getSocketByName(Recv),
      NameSend = chat_server_users:getName(Pid),
      gen_tcp:send(SocketRecv, "--msgFrom:" ++ NameSend ++ ">" ++ Text ++ "\n"),
      {ok, "ok\n"}
  end.

cmd_default(Data) ->
  io:format("[cmd_default] ~s~n",[Data]),
  {ok, Data ++ "\n"}.

pattern_user_setname(Data) ->
  case re:run(Data, "<<user,setname>>.+") of
    {match, _} -> true;
    nomatch -> false
  end.

is_valid_name(S) ->
  Strimmed = string:trim(S),
  case re:run(Strimmed, "^[a-zA-Z0-9]{3,}+$") of
    {match, _} -> true;
    nomatch -> false
  end.

pattern_user_list(Data) ->
  case re:run(Data, "<<user,list>>.*") of
    {match, _} -> true;
    nomatch -> false
  end.

pattern_user_whoami(Data) ->
  case re:run(Data, "<<user,whoami>>.*") of
    {match, _} -> true;
    nomatch -> false
  end.

pattern_msg(Data) ->
  case re:run(Data, "<<msg,[a-zA-Z0-9]{3,}+>>+") of
    {match, _} -> true;
    nomatch -> false
  end.

client_disconnection(Pid) ->
  chat_server_users:delete(Pid),
  io:format("error - closed ~w~n", [Pid]).
  
