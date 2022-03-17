-module(chat_server_socket).
-behavior(gen_server).
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([start/2, accept_loop/1, sendByPid/2]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

-record(server_state, {
		port,
		ip=any,
		lsocket=null}).

start(Name, Port) ->
	State = #server_state{port = Port},
	gen_server:start_link({local, Name}, ?MODULE, State, []).

init(State = #server_state{port=Port}) ->
	case gen_tcp:listen(Port, ?TCP_OPTIONS) of
   		{ok, LSocket} ->
   			NewState = State#server_state{lsocket = LSocket},
   			{ok, accept(NewState)};
   		{error, Reason} ->
   			{stop, Reason}
	end.

handle_cast({accepted, Pid, Socket}, State=#server_state{}) ->
  io:format("connected~n", []),
  io:format("~w~n", [Pid]),
  chat_server_users:putSocket(Pid, Socket),
	{noreply, accept(State)}.

accept_loop({Server, LSocket}) ->
	{ok, Socket} = gen_tcp:accept(LSocket),
  io:format("~w~n",[Socket]),
	gen_server:cast(Server, {accepted, self(), Socket}),
	loop(Socket, self()).
	
accept(State = #server_state{lsocket=LSocket}) ->
	proc_lib:spawn_link(?MODULE, accept_loop, [{self(), LSocket}]),
	State.

loop(Socket, Pid) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Data} ->
      io:format("~s", [Data]),
      case chat_server_manager:manage_cmd_get_resp(Data, Pid) of
        {ok, Resp} -> gen_tcp:send(Socket, Resp);
        _ -> ok
      end,
      loop(Socket, Pid);
    {error, closed} ->
      client_disconnection(Pid),
      ok
  end.

sendByPid([], Resp) ->
  ok;

sendByPid([H|T], Resp) ->
  Socket = chat_server_users:getSocketByPid(H),
  gen_tcp:send(Socket, Resp),
  sendByPid(T, Resp).

client_disconnection(Pid) ->
  chat_server_users:delete(Pid),
  chat_server_rooms:delete_pid(Pid),
  io:format("error - closed ~w~n", [Pid]).

handle_call(_Msg, _Caller, State) -> {noreply, State}.
handle_info(_Msg, Library) -> {noreply, Library}.
terminate(_Reason, _Library) -> ok.
code_change(_OldVersion, Library, _Extra) -> {ok, Library}.