-module(chat_server_users).
-behavior(gen_server).
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([start/0, get/1, getName/1, getAllNames/0, nameIsSet/1, nameIsSetByName/1, getSocketByName/1, getSocketByPid/1, getPidByName/1, get_state/0, put/2, putSocket/2, putName/2, delete/1]).

start() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get(Key) ->
  gen_server:call(?MODULE, {get, Key}).

getName(Key) ->
  gen_server:call(?MODULE, {getName, Key}).

nameIsSet(Key) ->
  Name = gen_server:call(?MODULE, {getName, Key}),
  io_lib:char_list(Name).

nameIsSetByName(Name) ->
  Resp = get_state(),
  {current_state, State} = Resp,
  L=[X || #{name := X} <- maps:values(State)],
  lists:member(Name,L).

getAllNames() ->
  Resp = get_state(),
  {current_state, State} = Resp,
  Pred = fun(K,V) -> 
    io_lib:char_list(maps:get(name, V, undefined)) 
  end,
  Filtered = maps:filter(Pred,State),
  ListOfSocketName = maps:values(Filtered),
  Names = [X || #{name := X} <- ListOfSocketName],
  Names.

getSocketByName(Name) ->
  Resp = get_state(),
  {current_state, State} = Resp,
  L=[Y || #{name := X, socket := Y} <- maps:values(State), X == Name],
  lists:nth(1, L).

getSocketByPid(Key) ->
  Resp = get_state(),
  {current_state, State} = Resp,
  Obj = maps:get(Key, State, undefined),
  maps:get(socket, Obj, undefined).

getPidByName(Name) ->
  Resp = get_state(),
  {current_state, State} = Resp,
  Pred = fun(K,V) -> 
    maps:get(name, V, undefined) == Name
  end,
  Filtered = maps:filter(Pred,State),
  L = maps:keys(Filtered),
  lists:nth(1, L).

get_state() ->
    gen_server:call(?MODULE, {get_state}).

put(Key, Value) ->
    gen_server:cast(?MODULE, {put, {Key, Value}}).

% on new connection
putSocket(Key, Value) ->
    gen_server:cast(?MODULE, {putSocket, {Key, Value}}).

putName(Key, Value) ->
    gen_server:cast(?MODULE, {putName, {Key, Value}}).

delete(Key) ->
    gen_server:cast(?MODULE, {delete, Key}).

init([]) ->
  {ok, #{}}.

handle_call({get, Key}, _From, State) ->
    Response = maps:get(Key, State, undefined),
    {reply, Response, State};

handle_call({getName, Key}, _From, State) ->
    Obj = maps:get(Key, State, undefined),
    Response = maps:get(name, Obj, undefined),
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

handle_cast({putSocket, {Key, Value}}, State) ->
    % NewState = maps:put(Key, Value, State),
    NewState = State#{Key => #{socket => Value, name => undefined}},
    {noreply, NewState};

handle_cast({putName, {Key, Value}}, State) ->
    % NewState = maps:put(Key, Value, State),
    Com=maps:get(Key, State),
    NewState = State#{Key => Com#{name := Value}},
    {noreply, NewState};

handle_cast({delete, Key}, State) ->
    NewState = maps:remove(Key, State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, Library) -> {noreply, Library}.
terminate(_Reason, _Library) -> ok.
code_change(_OldVersion, Library, _Extra) -> {ok, Library}.