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
  if
    Pattern_user_setname ->
      cmd_user_setname(Pid, Data);
    Pattern_user_list_res ->
      cmd_user_list();
    Pattern_user_whoami ->
      cmd_user_whoami(Pid);
    % pattern_msg(Data) ->
    %   io:format("~p~n",[Data]),
    %   ok;
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
  S = chat_server_users:get_state(),
  io:format("~p~n",[S]),
  {current_state, State} = S,
  Pred = fun(K,V) -> io_lib:char_list(V) end,
  Filtered = maps:filter(Pred,State),
  Keys = maps:values(Filtered),
  io:format("~p~n",[Keys]),
  Resp = "users:" ++ string:join(Keys,",") ++ "\n",
  io:format("~s~n",[Resp]),
  {ok, Resp}.

cmd_user_setname(Pid, Data) ->
  N1 = re:replace(Data, "<<user,setname>>", "", [{return,list}]),
  N2 = string:trim(N1),
  Ntest = is_valid_name(N2),
  % check if already taken
  if
    true ->
      chat_server_users:put(Pid, N2),
      {ok, "ok\n"};
    false ->
      {ok, "name is not valid; at least 3 chars of letters/numbers"}
  end.

cmd_user_whoami(Pid) ->
  Val = chat_server_users:get(Pid),
  case io_lib:char_list(Val) of
    true -> {ok, Val ++ "\n"};
    false -> {ok, "name is not set yet\n"}
  end.

cmd_default(Str) ->
  io:format("[str] ~s~n",[Str]),
  {ok, Str ++ "\n"}.

pattern_user_setname(Data) ->
  case re:run(Data, "<<user,setname>>.+") of
    {match, _} -> true;
    nomatch -> false
  end.

is_valid_name(S) ->
  Strimmed = string:trim(S),
  case re:run(Strimmed, "^[a-zA-Z0-9_.-]{3,}.*$") of
    {match, _} -> true;
    nomatch -> false
  end.
% add control not already taken

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

% pattern_msg(Data) ->
%   case re:run(Data, RegexMsg) of
%     {match, _} -> true;
%     nomatch -> false
%   end.

client_disconnection(Pid) ->
  chat_server_users:delete(Pid),
  io:format("error - closed ~w~n", [Pid]).
  
