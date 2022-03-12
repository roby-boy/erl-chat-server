-module(chat_server_manager).
-export([start/0, loop/2]).

start() ->
  chat_server_socket:start(?MODULE, 7000, {?MODULE, loop}).

loop(Socket, Pid) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            io:format("~s", [Data]),
            gen_tcp:send(Socket, Data),
            loop(Socket, Pid);
        {error, closed} ->
            io:format("error - closed ~w~n", [Pid]),
            ok
    end.
