-module(chat_server_socket).
-behavior(gen_server).
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([start/3, accept_loop/1]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

-record(server_state, {
		port,
		loop,
		ip=any,
		lsocket=null}).

start(Name, Port, Loop) ->
	State = #server_state{port = Port, loop = Loop},
	gen_server:start_link({local, Name}, ?MODULE, State, []).

init(State = #server_state{port=Port}) ->
	case gen_tcp:listen(Port, ?TCP_OPTIONS) of
   		{ok, LSocket} ->
   			NewState = State#server_state{lsocket = LSocket},
   			{ok, accept(NewState)};
   		{error, Reason} ->
   			{stop, Reason}
	end.

handle_cast({accepted, _Pid}, State=#server_state{}) ->
  io:format("connected~n", []),
  io:format("~w~n", [_Pid]),
	{noreply, accept(State)}.

accept_loop({Server, LSocket, {M, F}}) ->
	{ok, Socket} = gen_tcp:accept(LSocket),
	gen_server:cast(Server, {accepted, self()}),
	M:F(Socket, self()).
	
accept(State = #server_state{lsocket=LSocket, loop = Loop}) ->
	proc_lib:spawn(?MODULE, accept_loop, [{self(), LSocket, Loop}]),
	State.

handle_call(_Msg, _Caller, State) -> {noreply, State}.
handle_info(_Msg, Library) -> {noreply, Library}.
terminate(_Reason, _Library) -> ok.
code_change(_OldVersion, Library, _Extra) -> {ok, Library}.