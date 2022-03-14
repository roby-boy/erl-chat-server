-module(chat_server_manager).
-export([start/0, loop/2]).

start() ->
  chat_server_socket:start(?MODULE, 7000, {?MODULE, loop}).

loop(Socket, Pid) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            io:format("~s", [Data]),
            parse_command(Data, Pid),
            gen_tcp:send(Socket, Data),
            loop(Socket, Pid);
        {error, closed} ->
            client_disconnection(Pid),
            ok
    end.

parse_command(Data, Pid) ->
  % Cmd_user_setname = "<<user,setname>>",
  % Cmd_user_list = "<<user,list>>",
  % % wip
  % "<<user,disconnect>>",
  % "<<msg,send>>",
  % "<<room,list>>",
  % "<<room,create>>",
  % "<<room,leave>>",
  % "<<room,delete>>",
  Str = binary_to_list(string:chomp(Data)),
  case Str of
    "<<user,setname>>" ++ Name ->
      cmd_user_setname(Pid, Name);
    "<<user,list>>" ++ B ->
      cmd_user_list();
    _ -> 
      io:format("[cmd] _~n",[]),
      ok
  end.

cmd_user_list() ->
  io:format("[cmd] <<user,list>>~n",[]),
  S = chat_server_users:get_state(),
  io:format("~p~n",[S]).

cmd_user_setname(Pid, Name) ->
  io:format("[cmd] name: ~s~n",[Name]),
  chat_server_users:put(Pid, Name).

client_disconnection(Pid) ->
  chat_server_users:delete(Pid),
  io:format("error - closed ~w~n", [Pid]).
  
