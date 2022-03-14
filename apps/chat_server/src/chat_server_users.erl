-module(chat_server_users).
-behavior(gen_server).
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([start/0, get/1, get_state/0, put/2, delete/1]).

start() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get(Key) ->
    gen_server:call(?MODULE, {get, Key}).

get_state() ->
    gen_server:call(?MODULE, {get_state}).

put(Key, Value) ->
    gen_server:cast(?MODULE, {put, {Key, Value}}).

delete(Key) ->
    gen_server:cast(?MODULE, {delete, Key}).

init([]) ->
  {ok, #{}}.

handle_call({get, Key}, _From, State) ->
    Response = maps:get(Key, State, undefined),
    {reply, Response, State};

handle_call({get_state}, _From, State) ->
    Response = {current_state, State},
    {reply, Response, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({put, {Key, Value}}, State) ->
    NewState = maps:put(Key, Value, State),
    {noreply, NewState};

handle_cast({delete, Key}, State) ->
    NewState = maps:remove(Key, State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, Library) -> {noreply, Library}.
terminate(_Reason, _Library) -> ok.
code_change(_OldVersion, Library, _Extra) -> {ok, Library}.