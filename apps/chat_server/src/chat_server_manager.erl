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
  case Str of
    "<<user,setname>>" ++ Name ->
      cmd_user_setname(Pid, Name);
    "<<user,list>>" ++ B ->
      cmd_user_list();
    "<<user,whoami>>" ++ C ->
      cmd_whoami(Pid);
    _ ->
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

cmd_user_setname(Pid, Name) ->
  io:format("[cmd] name: ~s~n",[Name]),
  chat_server_users:put(Pid, Name),
  {ok, "ok\n"}.

cmd_whoami(Pid) ->
  Val = chat_server_users:get(Pid),
  case io_lib:char_list(Val) of
    true -> {ok, Val ++ "\n"};
    false -> {ok, "name is not set yet\n"}
  end.

cmd_default(Str) ->
  io:format("[str] ~s~n",[Str]),
  {ok, Str ++ "\n"}.

client_disconnection(Pid) ->
  chat_server_users:delete(Pid),
  io:format("error - closed ~w~n", [Pid]).
  
